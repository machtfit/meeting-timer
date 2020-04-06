(ns countdown-clock.main
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [goog.string.format]
            [goog.string :refer (format)]
            [clojure.string :as s]))

(defn timestamp []
  (/ (.getTime (js/Date.)) 1000.0))

                                        ; subs

(rf/reg-sub
 :get
 (fn [db [_ key]]
   (key db)))

                                        ; events

(rf/reg-event-db
 :initialize-db
 (fn [db _]
   (merge {:duration 300
           :running? false} db)))

(rf/reg-event-db
 :set
 (fn [db [_ key value]]
   (assoc db key value)))

(defn stop-interval [db]
  (let [previous-timer (:interval-timer db)]
    (when previous-timer (js/clearInterval previous-timer))
    (dissoc db :interval-timer)))

(defn set-interval [db]
  (-> db
      (stop-interval)
      (assoc :interval-timer (js/setInterval #(rf/dispatch [:tick]) 200))))

(rf/reg-event-db
 :start
 (fn [db _]
   (-> db
       (assoc :running? true)
       (assoc :passed-time 0)
       (assoc :start-time (timestamp))
       (set-interval))))

(rf/reg-event-db
 :stop
 (fn [db _]
   (-> db
       (assoc :running? false)
       (stop-interval))))

(rf/reg-event-db
 :tick
 (fn [db _]
   (assoc db :passed-time (- (timestamp) (:start-time db)))))
                                        ; views

(defn integer-field [key]
  [:input {:type      "input"
           :value     @(rf/subscribe [:get key])
           :on-change (fn [e] (rf/dispatch [:set key (int (-> e .-target .-value))]))}])


(defn clock []
  (let [duration       @(rf/subscribe [:get :duration])
        passed-time    @(rf/subscribe [:get :passed-time])
        time-left      (- duration passed-time)
        progress       (min 1.0 (/ passed-time duration))
        color          (condp < progress
                         0.98 "#d22"
                         0.9  "#e62e73"
                         0.5  "#fba842"
                         "#fbd872")
        mins           (int (/ (Math/abs time-left) 60))
        secs           (int (rem (Math/abs time-left) 60))
        radius         49
        angle          (* 359.9999 progress)
        arc-x          (* radius (Math/cos (* (- angle 90) (/ Math/PI 180.0))))
        arc-y          (* radius (Math/sin (* (- angle 90) (/ Math/PI 180.0))))
        large-arc-flag (if (> angle 180) 1 0)]
    [:g {:transform "translate(50, 50)"}
     [:circle {:cx    0
               :cy    0
               :r     radius
               :style {:stroke       "none"
                       :stroke-width 0.2
                       :fill         "#34b78f"}}]
     (when passed-time
       [:path {:d     (s/join " " (map str ["M" 0 (- radius) "A" radius radius 0 large-arc-flag 1 arc-x arc-y "L" 0 0 "Z"]))
               :style {:stroke-width 0.2
                       :stroke       "none"
                       :fill         color}}])
     (when passed-time
       [:text {:y           6
               :text-anchor "middle"
               :style       {:fill      "#84d7cf"
                             :font-size 25}}
        (str (when (neg? time-left) "-") (format "%d:%02d" mins secs))])]))

(defn app []
  (fn []
    [:div
     {:style {:width "100%"}}
     [:div
      "duration" [integer-field :duration]
      [:button {:on-click #(rf/dispatch [:start])} "start"]
      [:button {:on-click #(rf/dispatch [:stop])} "stop"]]
     [:svg {:style               {:width  "100%"
                                  :height "90vh"}
            :viewBox             "0 0 100 100"
            :preserveAspectRatio "xMidYMid meet"}
      [clock]]]))

(defn stop []
  (println "Stopping..."))

(defn start
  []
  (rf/dispatch-sync [:initialize-db])
  (r/render [app]
            (.getElementById js/document "app")))

(defn ^:export init []
  (start))
