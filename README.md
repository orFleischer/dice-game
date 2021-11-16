# Starting a New Elm Project in Intellij with the Elm Plugin

### How to Run
1. Either run `elm make src/DiceGame.elm --output=diceGame.js`  
   then open the `index.html` file in your browser.

2. Use `elm-live` to automatically compile and run
   1. First install `elm-live` via *NPM*. Use `npm install elm-live`.  
   This will install `elm-live` locally on your project. You will see a new folder called`node-modules`
   2. Run `elm-live` from `node_modules` folder using  
      `./node_modules/elm-live/bin/elm-live.js src/DiceGame.elm -- --output=diceGame.js`