
# Crave Control Frontend

## Prerequisites
[Elm](https://elm-lang.org/docs)

## How to build
 - cd `src/`
 - `elm make Main.elm --output ../public/js/app.js --debug`

## How to run in development mode
- From the `public` directory, run a web server locally
  - For example, [`python3 -m http.server 8000`](https://docs.python.org/3/library/http.server.html#http.server.SimpleHTTPRequestHandler) will run a local web server that serves on port 8000
- Visit http://localhost:8000 in your browser

Another option is to use [elm-live](https://www.elm-live.com/) as a dev server with live reload.
- From the `src` directory, run
`elm-live Main.elm --open --pushstate --dir=../public -- --output=../public/js/app.js --debug`
- Visit http://localhost:8000 in your browser
