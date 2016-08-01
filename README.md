# ngx-http-tagpurge

An nginx module to provide tag-based cache purging.

### How it works

1. After the response of a proxy upstream, get the "Cache-Tag"
   response header or whatever is configured in
   `tagpurge_cache_tag_header`. This header can get one or more tags
   separated by spaces.

2. Get the cache key associated with this upstream response. Based on
   this, and on the proxy cache configuration, the cache file path for
   the response is guessed.

3. Put the cache file path in a per-tag file, stored in the folder
   configured in `tagpurge_cache_path`.

4. Keep putting new cache file paths in the per-tag files, without
   duplicates.

5. An HTTP API listens for tags to purge. When a tag is purged, all
   the paths specified in the tag file are deleted.

### Known issues

- The lock used to write to files is maybe not shared across workers,
  double check that.
- The tests for the HTTP API don't pass because run-program somehow
  exits immediately, although there's no error.
- Write a comprehensive README and documentation.
- Think about handling expired cache entries.

### License

See the [license](LICENSE.md) file.
