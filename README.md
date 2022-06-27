This builds on examples and code from https://package.elm-lang.org/packages/joakin/elm-canvas/latest/
Thanks!

Setup:
Use elm-live from https://www.npmjs.com/package/elm-live

Command to build with hot-reload
elm-live src/Ball.elm --open -- --output=ball.js --debug

For building final version
elm-live src/Ball.elm --open -- --output=ball.js

Files that should be zipped
ball.js
elm-canvas.js
elm.json
index.html