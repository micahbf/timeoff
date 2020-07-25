# Time Off Planner

This is a very simple tool built in [ClojureScript](https://clojurescript.org/) using [Reagent](https://reagent-project.github.io/).

The entire app is in [src/timeoff/core.cljs](src/timeoff/core.cljs).

### Development mode
To start the Figwheel compiler, navigate to the project folder and run the following command in the terminal:

```
lein figwheel
```

Figwheel will automatically push cljs changes to the browser.
Once Figwheel starts up, you should be able to open the `public/index.html` page in the browser.

### REPL

The project is setup to start nREPL on port `7002` once Figwheel starts.
Once you connect to the nREPL, run `(cljs)` to switch to the ClojureScript REPL.

### Building for production

```
lein clean
lein package
```
