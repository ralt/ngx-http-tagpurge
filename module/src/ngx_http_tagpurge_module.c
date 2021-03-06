#include <nginx.h>
#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_http.h>

#define log(...) ngx_log_error(NGX_LOG_ERR, r->connection->log, 0, __VA_ARGS__);

ngx_module_t ngx_http_tagpurge_module;

extern ngx_module_t ngx_http_proxy_module;

typedef struct
{
	ngx_array_t *caches; /* ngx_http_file_cache_t */
} ngx_http_proxy_main_conf_t;

typedef struct
{
	ngx_str_t cache_tag_header;
	ngx_path_t *cache_path;
	ngx_shmtx_t mutex;
} ngx_http_tagpurge_main_conf_t;

static ngx_http_output_header_filter_pt ngx_http_next_header_filter;
static ngx_int_t
write_cache_tag_path(ngx_http_request_t *r,
		     ngx_http_tagpurge_main_conf_t *hmcf,
		     u_char *tag,
		     size_t tag_len);

/*
  Taken on: https://www.nginx.com/resources/wiki/start/topics/examples/headers_management/
*/
static ngx_table_elt_t *
search_headers_out(ngx_http_request_t *r, u_char *name, size_t len) {
	ngx_list_part_t            *part;
	ngx_table_elt_t            *h;
	ngx_uint_t                  i;

	/*
	  Get the first part of the list. There is usual only one part.
	*/
	part = &r->headers_out.headers.part;
	h = part->elts;

	/*
	  Headers list array may consist of more than one part,
	  so loop through all of it.
	*/
	for (i = 0; /* void */ ; i++) {
		if (i >= part->nelts) {
			if (part->next == NULL) {
				/* The last part, search is done. */
				break;
			}

			part = part->next;
			h = part->elts;
			i = 0;
		}

		/*
		  Just compare the lengths and then the names case
		  insensitively.
		*/
		if (len != h[i].key.len ||
		    ngx_strcasecmp(name, h[i].key.data) != 0) {
			/* This header doesn't match. */
			continue;
		}

		/*
		  Ta-da, we got one!
		  Note, we've stopped the search at the first matched header
		  while more than one header may fit.
		*/
		return &h[i];
	}

	/*
	  No headers was found.
	*/
	return NULL;
}

static ngx_int_t
ngx_http_tagpurge_filter(ngx_http_request_t *r)
{
	if (r->cache == NULL) {
		/* Cache disabled. */
		return ngx_http_next_header_filter(r);
	}

	ngx_http_tagpurge_main_conf_t *hmcf;
	hmcf = ngx_http_get_module_main_conf(r,
					     ngx_http_tagpurge_module);

	ngx_table_elt_t *upstream_header;
	upstream_header = search_headers_out(
		r,
		hmcf->cache_tag_header.data,
		hmcf->cache_tag_header.len);
	if (upstream_header == NULL) {
		return ngx_http_next_header_filter(r);
	}

	/* At this point, we need to get the list of tags.
	   Once that is gotten, for each tag, get the matching
	   tag file and put the cache key in it, if it doesn't
	   already exist in it. */
	ngx_str_t ngx_tags = upstream_header->value;

	/* Make a copy, so that we don't act on the response header. */
	u_char *tags = ngx_pstrdup(r->pool, &ngx_tags);

	/* ngx_pstrdup is handy, but it doesn't put the NUL byte
	   in dst. */
	tags[ngx_tags.len] = '\0';

	size_t tags_len = ngx_strlen(tags);

	u_char *tag;
	tag = ngx_palloc(r->pool, tags_len);
	if (tag == NULL) {
		return NGX_ERROR;
	}

	tag = (u_char *) strtok((char *) tags, " ");
	while (tag != NULL) {
		write_cache_tag_path(r, hmcf, tag, ngx_strlen(tag));
		tag = (u_char*) strtok(NULL, " ");
	}

	return ngx_http_next_header_filter(r);
}

static ngx_int_t
write_cache_tag_path(ngx_http_request_t *r,
		     ngx_http_tagpurge_main_conf_t *hmcf,
		     u_char *tag,
		     size_t tag_len)
{
	ngx_http_proxy_main_conf_t *pmcf;
	pmcf = ngx_http_get_module_main_conf(r,
					     ngx_http_proxy_module);

	ngx_http_file_cache_t *cache;
	cache = pmcf->caches->elts;

	ngx_file_t *file;
	file = ngx_palloc(r->pool, sizeof(ngx_file_t));
	if (file == NULL) {
		return NGX_ERROR;
	}

	file->log = ngx_palloc(r->pool, sizeof(ngx_log_t));
	if (file->log == NULL) {
		return NGX_ERROR;
	}

	file->name.len = hmcf->cache_path->name.len + 1 + tag_len;

	file->name.data = ngx_palloc(r->pool, file->name.len + 1);
	if (file->name.data == NULL) {
		return NGX_ERROR;
	}

	ngx_memcpy(file->name.data,
		   hmcf->cache_path->name.data,
		   hmcf->cache_path->name.len);

	(void) ngx_sprintf(file->name.data +
			   hmcf->cache_path->name.len,
			   "/%s", tag);

	file->name.data[file->name.len] = '\0';

	u_char *cache_path;
	size_t cache_path_len = cache->path->name.len + 1 +
		cache->path->len +
		2 * NGX_HTTP_CACHE_KEY_LEN + 1;

	cache_path = ngx_palloc(r->pool, cache_path_len);
	if (cache_path == NULL) {
		return NGX_ERROR;
	}

	ngx_memcpy(cache_path,
		   cache->path->name.data,
		   cache->path->name.len);

	u_char *p;
	p = cache_path + cache->path->name.len + 1 +
		cache->path->len;
	p = ngx_hex_dump(p,
			 r->cache->key,
			 NGX_HTTP_CACHE_KEY_LEN);
	*p = '\0';

	ngx_create_hashed_filename(cache->path,
				   cache_path,
				   cache_path_len - 1);

	/* The NUL byte isn't needed anymore, replace it
	   with a newline. */
	cache_path[cache_path_len - 1] = '\n';

	ngx_shmtx_lock(&hmcf->mutex);

	ngx_int_t rc;

	/* We only add the new cache key if it doesn't already exist. */
	file->fd = ngx_open_file(file->name.data,
				 NGX_FILE_RDONLY,
				 NGX_FILE_CREATE_OR_OPEN,
				 NGX_FILE_DEFAULT_ACCESS);

	if (file->fd == NGX_INVALID_FILE) {
		rc = NGX_ERROR;
		goto unlock;
	}

	ssize_t n;
	char c;
	size_t max_len = 4096;
	u_char *line;
	size_t read_bytes = 0;

	line = ngx_palloc(r->pool, max_len);
	if (line == NULL) {
		rc = NGX_ERROR;
		goto unlock;
	}

	size_t found = 0;

	for (;;) {
		/* ngx_read_file doesn't let you handle EOF
		   properly. It accepts an u_char, which _can't_
		   hold EOF, since its value is usually -1. */
		n = read(file->fd, &c, 1);
		if (n == -1) {
			rc = NGX_ERROR;
			goto unlock;
		}

		if (n == 0 || c == EOF) {
			break;
		}

		read_bytes += n;
		line[read_bytes - 1] = c;

		if (c != 0x0a) {
			continue;
		}

		if (ngx_strncmp(line,
				cache_path,
				cache_path_len) != 0) {
			/* Read the next line, reset read_bytes
			   for line[read_bytes - 1] at the next
			   iteration to work. */
			read_bytes = 0;
			continue;
		}

		/* Cache key was found. */
		found = 1;
		break;
	}

	if (ngx_close_file(file->fd) == NGX_FILE_ERROR) {
		rc = NGX_ERROR;
		goto unlock;
	}

	if (found) {
		rc = NGX_OK;
		goto unlock;
	}

	file->fd = ngx_open_file(file->name.data,
				 NGX_FILE_APPEND,
				 NGX_FILE_CREATE_OR_OPEN,
				 NGX_FILE_DEFAULT_ACCESS);

	if (file->fd == NGX_INVALID_FILE) {
		rc = NGX_ERROR;
		goto unlock;
	}

	size_t written_bytes = 0;

	for (;;) {
		n = ngx_write_fd(file->fd,
				 cache_path,
				 cache_path_len);
		if (n == -1) {
			ngx_log_error(NGX_LOG_ALERT, r->connection->log,
				      ngx_errno,
				      ngx_write_fd_n " \"%s\" failed",
				      file->name.data);
			rc = NGX_ERROR;
			goto unlock;
		}

		written_bytes += n;

		if (written_bytes == cache_path_len) {
			break;
		}
	}

	if (ngx_close_file(file->fd) == NGX_FILE_ERROR) {
		rc = NGX_ERROR;
		goto unlock;
	}

	rc = NGX_OK;

unlock:
	ngx_shmtx_unlock(&hmcf->mutex);

	return rc;
}

static ngx_int_t
ngx_http_tagpurge_init(ngx_conf_t *cf)
{
	ngx_http_next_header_filter = ngx_http_top_header_filter;
	ngx_http_top_header_filter = ngx_http_tagpurge_filter;

	return NGX_OK;
}

static void *
ngx_http_tagpurge_create_main_conf(ngx_conf_t *cf)
{
	ngx_http_tagpurge_main_conf_t *hmcf;

	hmcf = ngx_palloc(cf->pool, sizeof(ngx_http_tagpurge_main_conf_t));
	if (hmcf == NULL) {
		return NULL;
	}

	ngx_shmtx_sh_t *addr;
	addr = ngx_palloc(cf->pool, sizeof(ngx_shmtx_sh_t));
	if (addr == NULL) {
		return NULL;
	}

	u_char *mutex_name = (u_char *) "tagpurge";
	if (ngx_shmtx_create(&hmcf->mutex, addr, mutex_name) != NGX_OK) {
		return NULL;
	}

	return hmcf;
}

static char *
ngx_http_tagpurge_init_main_conf(ngx_conf_t *cf, void *conf)
{
	ngx_http_tagpurge_main_conf_t *hmcf = conf;

	if (hmcf->cache_tag_header.len == 0) {
		hmcf->cache_tag_header = (ngx_str_t) ngx_string("cache-tag");
	}

	return NGX_CONF_OK;
}

char *
ngx_http_tagpurge_cache_path_set_slot(ngx_conf_t *cf, ngx_command_t *cmd,
				      void *conf)
{
	ngx_http_tagpurge_main_conf_t *confp;
	confp = conf;

	confp->cache_path = ngx_palloc(cf->pool,
				      sizeof(ngx_path_t));
	if (confp->cache_path == NULL) {
		return NGX_CONF_ERROR;
	}

	ngx_str_t *value;
	value = cf->args->elts;

	confp->cache_path->name = value[1];
	if (confp->cache_path->name.data[confp->cache_path->name.len - 1] == '/') {
		/* Remove trailing slash if there. */
		confp->cache_path->name.len--;
	}

	if (ngx_add_path(cf, &confp->cache_path) != NGX_OK) {
		return NGX_CONF_ERROR;
	}

	return NGX_CONF_OK;
}

static ngx_command_t ngx_http_tagpurge_commands[] = {
	{ ngx_string("tagpurge_cache_tag_header"),
	  NGX_HTTP_MAIN_CONF|NGX_CONF_TAKE1,
	  ngx_conf_set_str_slot,
	  NGX_HTTP_MAIN_CONF_OFFSET,
	  offsetof(ngx_http_tagpurge_main_conf_t, cache_tag_header),
	  NULL},

	{ ngx_string("tagpurge_cache_path"),
	  NGX_HTTP_MAIN_CONF|NGX_CONF_TAKE1,
	  ngx_http_tagpurge_cache_path_set_slot,
	  NGX_HTTP_MAIN_CONF_OFFSET,
	  0,
	  NULL},

	ngx_null_command
};

static ngx_http_module_t ngx_http_tagpurge_module_ctx = {
	NULL,			/* preconfiguration */
	ngx_http_tagpurge_init, /* postconfiguration */
	ngx_http_tagpurge_create_main_conf, /* create main configuration */
	ngx_http_tagpurge_init_main_conf, /* init main configuration */
	NULL,                   /* create server configuration */
	NULL,                   /* merge server configuration */
	NULL,                   /* create location configuration */
	NULL                    /* merge location configuration */
};

ngx_module_t ngx_http_tagpurge_module = {
	NGX_MODULE_V1,
	&ngx_http_tagpurge_module_ctx,	/* module context */
	ngx_http_tagpurge_commands,	/* module directives */
	NGX_HTTP_MODULE,		/* module type */
	NULL,				/* init master */
	NULL,				/* init module */
	NULL,				/* init process */
	NULL,				/* init thread */
	NULL,				/* exit thread */
	NULL,				/* exit process */
	NULL,				/* exit master */
	NGX_MODULE_V1_PADDING
};
