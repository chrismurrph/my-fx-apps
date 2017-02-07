(ns my-trys.scene-graph-demo
  (:require [fn-fx.fx-dom :as dom]
            [fn-fx.controls :as ui]
            ))

(def black (ui/color :red 0 :blue 0 :green 0))
(def blue (ui/color :red 0 :blue 1 :green 0))

(defn -main []
  (let [u (ui/stage
            :shown true
            :title "JavaFX Scene Graph Demo"
            :scene (ui/scene
                     :fill black
                     :width 500
                     :height 500
                     :root (ui/group
                             :children [(ui/rectangle
                                          :x 25
                                          :y 25
                                          :width 250
                                          :height 250
                                          :fill blue)])))

        handler-fn (fn [evt]
                     (println "Received Event: " evt))]
    (dom/app u nil)))