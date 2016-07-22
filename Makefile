NGINX_VERSION=1.11.2
NGINX_FOLDER=vendor/nginx-$(NGINX_VERSION)
SOURCES:=$(wildcard src/*.c) $(wildcard src/*.h) config

all: build/nginx/sbin/nginx

build/nginx/sbin/nginx: $(NGINX_FOLDER)/.configured $(SOURCES) build/nginx
	cd $(NGINX_FOLDER) && \
	make -j16 && \
	make install

	ln -sfn $(PWD)/nginx.conf build/nginx/conf/nginx.conf

build/nginx:
	mkdir -p build/nginx/conf build/nginx/logs

$(NGINX_FOLDER)/.configured: $(NGINX_FOLDER)
	cd $(NGINX_FOLDER) && \
	CFLAGS="" ./configure \
		--with-debug \
		--prefix=$(PWD)/build/nginx \
		--conf-path=conf/nginx.conf \
		--error-log-path=logs/error.log \
		--http-log-path=logs/access.log \
		--add-module=../..
	touch $@

$(NGINX_FOLDER): vendor
	cd vendor && \
	curl -O http://nginx.org/download/nginx-$(NGINX_VERSION).tar.gz && \
	tar xf nginx-$(NGINX_VERSION).tar.gz && \
	rm -f nginx-$(NGINX_VERSION).tar.gz

vendor:
	mkdir -p vendor

.PHONY: tests

tests:
	sbcl --load tests/basic.lisp --quit
