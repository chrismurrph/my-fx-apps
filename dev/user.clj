(ns user
  (:require
    [clojure.tools.namespace.repl :refer [disable-reload! refresh clear set-refresh-dirs]]))

(set-refresh-dirs "src" "dev")
