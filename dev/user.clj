(ns user
  (:require
    [clojure.tools.namespace.repl :refer [disable-reload! refresh clear set-refresh-dirs]]
    [clojure.stacktrace :refer (print-stack-trace)]))

(set-refresh-dirs "src" "dev")
