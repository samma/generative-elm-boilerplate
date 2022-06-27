This builds on examples and code from https://package.elm-lang.org/packages/joakin/elm-canvas/latest/
Thanks!

Setup:
Use elm-live from https://www.npmjs.com/package/elm-live

Command to build with hot-reload
elm-live src/Ball.elm --open -- --output=ball.js --debug

Building a regular version
elm-live src/Ball.elm --open -- --output=ball.js

To build and optimize use, and make sure index.html points at ball.min.js
./optmize.sh src/Ball.elm

Files that should be zipped
ball.min.js
elm-canvas.js
elm.json
index.html

