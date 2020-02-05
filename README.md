# miracle.ui

A tool that connects to a socket-repl, watches all functions in a namespace, filters through function invocations, and lets you set a particular function call as the context for `eval`, allowing you to debug that function efficiently.

## Usage

```
git clone https://github.com/Saikyun/miracle-ui
cd miracle-ui
yarn

# leave this running
# i.e. `shadow-cljs watch renderer main test-stuff`
yarn watch

# new terminal when the build's completed
# i.e. `electron .`
yarn start
```
