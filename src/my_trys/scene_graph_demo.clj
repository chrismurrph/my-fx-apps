(ns my-trys.scene-graph-demo
  (:require [fn-fx.fx-dom :as dom]
            [fn-fx.controls :as ui]
            )
  (:import [javafx.animation TranslateTransition]))

(def black (ui/color :red 0 :blue 0 :green 0))
(def blue (ui/color :red 0 :blue 1 :green 0))

(def translate (ui/translate-transition
                 :to-x 390
                 :to-y 390))

(def r (ui/rectangle
         :x 25
         :y 25
         :width 250
         :height 250
         :fill blue))

(def transition (ui/parallel-transition r translate))

(defn -main []
  (let [u (ui/stage
            :shown true
            :title "JavaFX Scene Graph Demo"
            :scene (ui/scene
                     :fill black
                     :width 500
                     :height 500
                     :root (ui/group
                             :rotate 45
                             :children [r])))

        handler-fn (fn [evt]
                     (println "Received Event: " evt))]
    (dom/app u nil)))