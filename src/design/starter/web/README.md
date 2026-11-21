# __NAME__

A small website in Limen, in the design of limen-lang.org.

```
limen run main.lmn      # serve it on http://127.0.0.1:8080
limen test main.lmn     # run its tests
limen build main.lmn    # build one binary that serves the whole site
```

- `pages/` holds the pages, plain HTML. `layout.html` wraps each one: `$title`
  and `$content` are filled in by `page` in main.lmn. A new page is a file
  here and a route in `routes`.
- `assets/` is served under `/assets/`: `style.css` (the design's tokens and
  components; change it freely), the logo, the fonts and the theme switch.
- Pages and assets are read into the program when it is built, so the binary
  serves itself and needs no `fs` grant. Rebuild after changing them.
- A path with no route answers 404: browsers get an error page in the same
  design (`http.error_page`), other clients plain text.

To serve on another address, change both `address` in main.lmn and `listen`
in limen.toml.
