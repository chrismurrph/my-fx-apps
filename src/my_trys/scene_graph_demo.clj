(ns my-trys.scene-graph-demo
  (:require [fn-fx.fx-dom :as dom]
            [fn-fx.controls :as ui]
            [fn-fx.diff :refer [component defui-fx render]]
            [om-fx.next :refer [defui]]
            [fn-fx.controls :as controls]
            [fn-fx.fx-dom :as fx-dom])
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

(defui-fx TestControl
       (render [this {:keys [button-text] :as state}]
               (controls/border-pane
                 :top (controls/h-box
                        :padding (javafx.geometry.Insets. 15 12 15 12)
                        :spacing 10
                        :alignment (javafx.geometry.Pos/CENTER)
                        :children [(controls/button
                                     :text button-text
                                     :on-action {:event :press-button
                                                 :fn-fx/include {:fn-fx/event #{:target}}})
                                   #_(controls/check-box
                                     :text "Import first row as headers"
                                     :selected false
                                     :on-action {:event :toggle-option
                                                 :path [:csv :first-row-headers]})
                                   (controls/button
                                     :text "Reset"
                                     :on-action {:event :reset})]))))

(defui-fx Stage
       (render [this {:keys [button-text] :as state}]
               (controls/stage
                 :shown true
                 :title (str "JavaFX Scene Graph Demo")
                 :scene (controls/scene
                          :fill black
                          :width 500
                          :height 500
                          :root (test-control {:button-text button-text})))))

(def initial-state
  {:button-text "Initially"
   :times-pressed 0})

(defonce data-state (atom initial-state))

(defmulti handle-event (fn [_ {:keys [event]}]
                         event))

(defmethod handle-event :press-button
  [state {:keys [_]}]
  (as-> state $
      (update $ :times-pressed inc)
        (assoc $ :button-text (str "Pressed: " (:times-pressed $)))))

(defmethod handle-event :reset
  [state {:keys [_]}]
  initial-state)

(defn -main-2
  ([] (-main-2 {:button-text "Press me!"}))
  ([{:keys [button-text]}]
   (swap! data-state assoc :button-text button-text)
    (let [handler-fn (fn [event]
                       (println event)
                       (try
                         (swap! data-state handle-event event)
                         (catch Throwable exception
                           (println exception))))
          ui-state (agent (fx-dom/app (stage @data-state) handler-fn))]
      (add-watch data-state :ui (fn [_ _ _ _]
                                  (send ui-state
                                        (fn [old-ui]
                                          (println "-- State Updated --")
                                          (println @data-state)
                                          (fx-dom/update-app old-ui (stage @data-state)))))))))

(defn -main-1 []
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