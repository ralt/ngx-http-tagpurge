NGINX_VERSION=1.11.2
NGINX_FOLDER=vendor/nginx-$(NGINX_VERSION)
SOURCES:=$(wildcard module/src/*.c) $(wildcard module/src/*.h) module/config
PURGER_SOURCES:=$(wildcard purger/*.lisp) $(wildcard *.asd)
PURGER_DEPS:=$(wildcard *.asd)
ifdef CI
	COMPRESS=
else
	COMPRESS=--compress-core
endif

all: build/nginx/sbin/nginx build/purger/tagpurge-http-api

build/purger/tagpurge-http-api: $(PURGER_SOURCES) build/purger/deps build/purger/bin/buildapp build/purger/quicklocal/setup.lisp compress-core
	./build/purger/bin/buildapp \
		--asdf-tree build/purger/quicklocal/dists \
		--asdf-path purger/ \
		--load-system purger \
		--eval '(setf *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~A~%" c) (uiop:quit -1)))' \
		$(COMPRESS) \
		--output build/purger/tagpurge-http-api \
		--entry purger:main

compress-core:

build/purger/deps: build/purger/quicklocal/setup.lisp $(PURGER_DEPS)
	sbcl \
		--noinform \
		--noprint \
		--disable-debugger \
		--no-sysinit \
		--no-userinit \
		--load build/purger/quicklocal/setup.lisp \
		--eval '(push "$(PWD)/purger/" asdf:*central-registry*)' \
		--eval '(ql:quickload :purger)' \
		--eval '(quit)'
	touch $@

build/purger/bin/buildapp: build/purger/quicklocal/setup.lisp
	mkdir -p build/purger/bin
	cd $(shell sbcl --noinform --noprint --disable-debugger --no-sysinit --no-userinit --load build/purger/quicklocal/setup.lisp \
			--eval '(ql:quickload :buildapp :silent t)' \
			--eval '(format t "~A~%" (asdf:system-source-directory :buildapp))' \
			--eval '(quit)') && \
	$(MAKE) DESTDIR=$(PWD)/build/purger install

build/purger/quicklocal/setup.lisp: build/purger/quicklisp.lisp
	sbcl --noinform --noprint --disable-debugger --no-sysinit --no-userinit \
		--load build/purger/quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "build/purger/quicklocal/")' \
		--eval '(quit)'

build/purger/quicklisp.lisp:
	mkdir -p build/purger
	cd build/purger && wget https://beta.quicklisp.org/quicklisp.lisp
	cd build/purger && echo '4a7a5c2aebe0716417047854267397e24a44d0cce096127411e9ce9ccfeb2c17 *quicklisp.lisp' | shasum -c -

build/nginx/sbin/nginx: $(NGINX_FOLDER)/.configured $(SOURCES) build/nginx
	cd $(NGINX_FOLDER) && \
	make -j16 && \
	make install

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
		--add-module=../../module
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
	sbcl --disable-debugger --load tests/basic.lisp --quit
