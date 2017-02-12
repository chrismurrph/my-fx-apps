(ns my-trys.scene-graph-demo
  (:require [fn-fx.fx-dom :as dom]
            [fn-fx.controls :as ui]
            [fn-fx.diff :refer [component defui-fx render]]
            [om-fx.next :as om :refer [defui]]
            [fn-fx.controls :as controls]
            [fn-fx.fx-dom :as fx-dom]
            [om-fx.util :as u])
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
                                     :on-action {:event :reset})
                                   (controls/button
                                     :text "Replace"
                                     :on-action {:event :replace})]))))

#_(defui MyCounter
       Object
       (render [this]
               (let [{:keys [count]} (om/props this)]
                 (controls/button
                   :text "Counter"
                   :on-action {:event :wont-work}))))
#_(def my-counter (om/factory MyCounter))

(defui-fx MyCounter
          (render [this {:keys [] :as state}]
                  (controls/button
                    :text "Counter"
                    :on-action {:event :replace})))

(def om-app-state (atom {:count 0}))

(def reconciler
  (om/reconciler {:state om-app-state}))

(defui MyOmCounter
       Object
       (render [this]
               (let [{:keys [count]} (om/props this)]
                 (println (str "To render: " count))
                 (controls/button
                   :text "Counter"
                   :on-action {:event :replace}))))

(defn -main-3 []
  ;(events-driver #(main-stage %))
  (om/add-root! reconciler
                MyOmCounter (fn [] (assert false "Don't need a root"))))

(defui-fx EmptyControl
          (render [this {:keys [] :as state}]
                  (controls/label
                    :text "")))

(def my-stage (atom nil))

(defn create-stage [button-text replaced-mode]
  (let [the-stage (controls/stage
                    :shown true
                    :title (str "JavaFX Scene Graph Demo, replaced: " replaced-mode)
                    :scene (controls/scene
                             :fill black
                             :width 500
                             :height 500
                             :root (case replaced-mode
                                     :counter (my-counter)
                                     :test (u/probe-on (test-control {:button-text button-text}))
                                     :nothing (empty-control))))]
    (reset! my-stage the-stage)
    the-stage))

(defui-fx MainStage
       (render [this {:keys [button-text replaced-mode] :as state}]
               (create-stage button-text replaced-mode)))

(def initial-state
  {:button-text "Initially"
   :times-pressed 0
   :replaced-mode :nothing})

(defonce data-state (atom initial-state))

(defn reset []
  (reset! data-state initial-state))

;;
;; This is what assignment of root could do
;;
(defn give-life-1 []
  (swap! data-state assoc :replaced-mode :test))

;;
;; does not work b/c stage is not map, so can't :scene on it
;;
(defn give-life-2 []
  (reset! my-stage (create-stage "some text" :test))
  (println (str "stage is a " (type @my-stage)))
  (swap! my-stage assoc-in [:scene :root] (test-control {:button-text "Anything"})))

;;
;; works fine b/c :scsns exists in a map!
;;
(defn give-life-3 []
  (swap! (atom {:scene {:root nil}}) assoc-in [:scene :root] (test-control {:button-text "Anything"})))

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

(defmethod handle-event :replace
  [state {:keys [_]}]
  (update state :replaced-mode {:counter :test
                                :test :counter}))

;(defui HelloWorld
;       Object
;       (render [this]
;               (println "Hello, world!")))
;
;(def hello (om/factory HelloWorld))

(defn events-driver [main-stage-fn]
  (let [handler-fn (fn [event]
                     (println event)
                     (try
                       (swap! data-state handle-event event)
                       (catch Throwable exception
                         (println exception))))
        ui-state (agent (fx-dom/app (main-stage-fn @data-state) handler-fn))]
    (add-watch data-state :ui (fn [_ _ _ _]
                                (send ui-state
                                      (fn [old-ui]
                                        (println "-- State Updated --")
                                        (println @data-state)
                                        (fx-dom/update-app old-ui (main-stage-fn @data-state))))))))

(defn -main-2
  ([] (-main-2 {:button-text "Press me!"}))
  ([{:keys [button-text]}]
   (swap! data-state assoc :button-text button-text)
    (events-driver #(main-stage %))))

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