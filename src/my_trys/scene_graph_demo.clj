(ns my-trys.scene-graph-demo
  (:require [fn-fx.fx-dom :as dom]
            [fn-fx.controls :as ui])
  (:import [javafx.animation TranslateTransition ParallelTransition Timeline]
           [javafx.util Duration]))

(def black (ui/color :red 0 :blue 0 :green 0))
(def blue (ui/color :red 0 :blue 1 :green 0))

(defn create-translate []
  (let [trans (TranslateTransition. (Duration/millis 750))]
    (doto trans
      (.setToX 390)
      (.setToY 390))))

;;
;; FillTransition fill = new FillTransition(Duration.millis(750));
;; fill.setToValue(Color.RED);



;; RotateTransition rotate = new RotateTransition(Duration.millis(750));
;; rotate.setToAngle(360);

;; ScaleTransition scale = new ScaleTransition(Duration.millis(750));
;; scale.setToX(0.1);
;; scale.setToY(0.1);
;;

(defn run-p-transition [rect translate]
  (assert rect)
  (assert translate)
  (let [p-transition (ParallelTransition. rect (into-array [translate]))]
    (doto p-transition
      (.setCycleCount Timeline/INDEFINITE)
      (.setAutoReverse true)
      (.play))))

(def rect (ui/rectangle
         :x 25
         :y 25
         :width 250
         :height 250
         :fill blue))

;(def transition (ui/parallel-transition r translate))

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
                             :children [rect])))

        handler-fn (fn [evt]
                     (println "Received Event: " evt))]
    ;; Won't work b/c I can't get the original component out of rect
    ;(println "rect: " (.getNode rect))
    ;(run-p-transition (:dom-node rect) (create-translate) #_(create-fill) #_(create-rotate) #_(create-scale))
    (dom/app u nil)))