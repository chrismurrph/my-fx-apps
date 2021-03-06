(ns getting-started.02-form
  (:require [fn-fx.fx-dom :as dom]
            [fn-fx.diff :refer [component defui-fx render should-update?]]
            [fn-fx.controls :as ui]))

(def firebrick
  (ui/color :red 0.69 :green 0.13 :blue 0.13))

;; The main login window component, notice the authed? parameter, this defines a function
;; we can use to construct these ui components, named "login-form"
(defui-fx LoginWindow
       (render [this {:keys [authed?]}]
               (ui/grid-pane
                 :alignment :center
                 :hgap 10
                 :vgap 10
                 :padding (ui/insets
                            :bottom 25
                            :left 25
                            :right 25
                            :top 25)
                 :children [(ui/text
                              :text "Welcome"
                              :font (ui/font
                                      :family "Tahoma"
                                      :weight :normal
                                      :size 20)
                              :grid-pane/column-index 0
                              :grid-pane/row-index 0
                              :grid-pane/column-span 2
                              :grid-pane/row-span 1)

                            (ui/label
                              :text "User:"
                              :grid-pane/column-index 0
                              :grid-pane/row-index 1)

                            (ui/text-field
                              :id :user-name-field
                              :grid-pane/column-index 1
                              :grid-pane/row-index 1)

                            (ui/label :text "Password:"
                                      :grid-pane/column-index 0
                                      :grid-pane/row-index 2)

                            (ui/password-field
                              :id :password-field
                              :grid-pane/column-index 1
                              :grid-pane/row-index 2)

                            (ui/h-box
                              :spacing 10
                              :alignment :bottom-right
                              :children [(ui/button :text "Sign in"
                                                    :on-action {:event :auth
                                                                :fn-fx/include {:user-name-field #{:text}
                                                                                :password-field #{:text}}})]
                              :grid-pane/column-index 1
                              :grid-pane/row-index 4)

                            (ui/text
                              :text (if authed? "Sign in was pressed" "")
                              :fill firebrick
                              :grid-pane/column-index 1
                              :grid-pane/row-index 6)])))

;; Wrap our login form in a stage/scene, and create a "stage" function
(defui-fx Stage
       (render [this args]
               (ui/stage
                 :title "JavaFX Welcome"
                 :shown true
                 :scene (ui/scene
                          :root (login-window args)))))

(defn -main []
  (let [;; Data State holds the business logic of our app
        data-state (atom {:authed? false})

        ;; handler-fn handles events from the ui and updates the data state
        handler-fn (fn [{:keys [event] :as all-data}]
                     (println "UI Event" event all-data)
                     (case event
                       :auth (swap! data-state assoc :authed? true)
                       (println "Unknown UI event" event all-data)))

        ;; ui-state holds the most recent state of the ui
        ui-state (agent (dom/app (stage @data-state) handler-fn))]

    ;; Every time the data-state changes, queue up an update of the UI
    (add-watch data-state :ui (fn [_ _ _ _]
                                (send ui-state
                                      (fn [old-ui]
                                        (dom/update-app old-ui (stage @data-state))))))))

(comment
  (-main)
  )
