#include <nginx.h>
#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_http.h>

#define log(...) ngx_log_error(NGX_LOG_ERR, r->connection->log, 0, __VA_ARGS__);

ngx_module_t ngx_http_tagpurge_module;

typedef struct
{
	ngx_str_t cache_tag_header;
	ngx_path_t *cache_path;
} ngx_http_tagpurge_main_conf_t;

static ngx_http_output_header_filter_pt ngx_http_next_header_filter;

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
	/* For now, assume only one tag. */
	ngx_str_t tag = upstream_header->value;

	if (r->cache == NULL) {
		/* Cache disabled. */
		return ngx_http_next_header_filter(r);
	}

	ngx_file_t *file;
	file = ngx_palloc(r->pool, sizeof(ngx_file_t));
	if (file == NULL) {
		return NGX_ERROR;
	}

	file->name.len = hmcf->cache_path->name.len + 1 +
		hmcf->cache_path->len + tag.len;

	file->name.data = ngx_palloc(r->pool, file->name.len + 1);
	if (file->name.data == NULL) {
		return NGX_ERROR;
	}

	ngx_memcpy(file->name.data,
		   hmcf->cache_path->name.data,
		   hmcf->cache_path->name.len);

	(void) ngx_sprintf(file->name.data +
			   hmcf->cache_path->name.len,
			   "/%s", tag.data);

	if (ngx_create_path(file, hmcf->cache_path) != NGX_OK) {
		return NGX_ERROR;
	}

	file->fd = ngx_open_file(file->name.data,
				 NGX_FILE_RDWR,
				 NGX_FILE_CREATE_OR_OPEN,
				 NGX_FILE_DEFAULT_ACCESS);

	if (file->fd == NGX_INVALID_FILE) {
		goto end;
	}

	ssize_t n = 0;
	for (;;) {
		n += ngx_write_fd(file->fd, r->cache->key, ngx_strlen(r->cache->key));
		if (n == -1 ){
			ngx_log_error(NGX_LOG_ALERT, r->connection->log,
				      ngx_errno,
				      ngx_write_fd_n " \"%s\" failed",
				      file->name.data);
			goto close;
		}

		if ((size_t) n == ngx_strlen(r->cache->key)) {
			break;
		}
	}

close:
	if (ngx_close_file(file->fd) == NGX_FILE_ERROR) {
		goto end;
	}

end:
	return ngx_http_next_header_filter(r);
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
