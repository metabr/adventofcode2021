# Advent of Code 2021

## Run

In order to run single day's solution you need only clj installed.

    clj -X advent.dayXX/run

## Start REPL

To have `cider-clj`' alias you will need to install practicalli clojure deps first. 

    git clone git@github.com:practicalli/clojure-deps-edn.git ~/.clojure/
    
When you do, you can start the REPL and connect to in with `cider-connect-clj` if using CIDER.

    clojure -M:middleware/cider-clj
