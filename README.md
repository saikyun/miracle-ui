# miracle.ui

A tool that connects to a socket-repl, watches all functions in a namespace, filters through function invocations, and lets you set a particular function call as the context for `eval`, allowing you to debug that function efficiently.

## Usage

### To install and build
```
git clone https://github.com/Saikyun/miracle-ui
cd miracle-ui
yarn

# leave this running
# i.e. `shadow-cljs watch renderer main test-stuff`
yarn watch
```

### To try the functionality
Run test-stuff in a new terminal:
```
node test_stuff/main.js
#=> test-stuff main
#=> starting
```

Then, connect to the shadow-cljs repl server using your favourite tool:
```
;;=> shadow-cljs - REPL - see (help)
;;=> To quit, type: :repl/quit
shadow.user=> (shadow/repl :test-stuff)
cljs.user=> (in-ns 'test-stuff.core)
;;=> nil
test-stuff.core=> 
```

Start miracle.ui in a new terminal:
```
# i.e. `electron .`
yarn start
```

Then in the miracle.ui window, in the top text field, write `test-stuff.core`.

Then, in the repl connected to `:test-stuff`, call some functions:
```
test-stuff.core=>
 (do
    (ye 10)
    (ye 15)
    (ye 20)
    
    (ye1 10)
    (ye1 15)
    (ye1 20)
    
    (ye3 10)
    (ye3 15))
  
  (doseq [i (range 100)
          :let [x (rand-int i)]]
    (try
      (ye3 x)
      (catch js/Error e
        (println "nobody cares"))))
```

Look at the miracle.ui window. Try clicking the button with the var names.
Now you can see function calls and if the function threw an error (see ye3).
If you click a row, you will set that as the context.
To verify that it is set, run this in the repl:
```
test-stuff.core=> @miracle.save/context
;;=> {:key test-stuff.core/ye3, :id 10, :data {:miracle.save/ret 0, :miracle.save/args {x 0}, :miracle.save/arglist [x]}}
```

You can then run code in context like so:
```
test-stuff.core=> (miracle.save/eval-in-context '(do (+ x x)))
;;=> (cljs.core/let [{:syms [x]} (:miracle.save/args (:data (cljs.core/deref miracle.save/context)))] (do (+ x x)))
```
And if you feed the resulting form into the repl again (preferably automatically with your editor):
```
test-stuff.core=> (cljs.core/let [{:syms [x]} (:miracle.save/args (:data (cljs.core/deref miracle.save/context)))] (do (+ x x)))
2
```

### Tip
Replace `(+ x x)` in the above form with source code from your editor. I do it like this in emacs: `(format "(miracle.save/eval-in-context '(do %s))" expr)`), where `expr` is the expression at my cursor.
This way you can use `eval-last-expr` like usual, only that it's in the context of the function call that you selected. This way you can debug your function more efficiently.
