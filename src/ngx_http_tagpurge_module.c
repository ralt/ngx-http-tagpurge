#include "ddebug.h"

#include <nginx.h>
#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_http.h>
#include <stdio.h>

ngx_module_t ngx_http_tagpurge_module;

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
	u_char* upstream_header_name = ngx_palloc(r->pool, 3);
	upstream_header_name[0] = 'f';
	upstream_header_name[1] = 'o';
	upstream_header_name[2] = 'o';

	ngx_table_elt_t *upstream_header;
	upstream_header = search_headers_out(
		r,
		upstream_header_name,
		3);
	if (upstream_header == NULL) {
		return ngx_http_next_header_filter(r);
	}

	ngx_table_elt_t *new_header;
	new_header = ngx_list_push(&r->headers_out.headers);
	if (new_header == NULL) {
		return NGX_ERROR;
	}
	new_header->hash = 1;

	ngx_str_set(&new_header->key, "X-Foo");
	ngx_str_set(&new_header->value, upstream_header->value.data);

	return ngx_http_next_header_filter(r);
}

static ngx_int_t
ngx_http_tagpurge_init(ngx_conf_t *cf)
{
	ngx_http_next_header_filter = ngx_http_top_header_filter;
	ngx_http_top_header_filter = ngx_http_tagpurge_filter;

	return NGX_OK;
}


static ngx_http_module_t ngx_http_tagpurge_module_ctx = {
	NULL,			/* preconfiguration */
	ngx_http_tagpurge_init, /* postconfiguration */
	NULL,                   /* create main configuration */
	NULL,                   /* init main configuration */
	NULL,                   /* create server configuration */
	NULL,                   /* merge server configuration */
	NULL,                   /* create location configuration */
	NULL                    /* merge location configuration */
};

ngx_module_t ngx_http_tagpurge_module = {
	NGX_MODULE_V1,
	&ngx_http_tagpurge_module_ctx,	/* module context */
	NULL,				/* module directives */
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
