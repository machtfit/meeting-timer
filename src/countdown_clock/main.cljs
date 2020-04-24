(ns countdown-clock.main
  (:require [clojure.string :as s]
            [goog.string :refer [format]]
            [re-frame.core :as rf]
            [reagent.core :as r]))

(defn timestamp []
  (/ (.now js/Date) 1000.0))

                                        ; subs

(rf/reg-sub
 :get
 (fn [db [_ key]]
   (key db)))

                                        ; events

(def granularity 30)

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
      (assoc :last-tick (timestamp))
      (assoc :interval-timer (js/setInterval #(rf/dispatch [:tick]) granularity))))

(rf/reg-event-db
 :resume
 (fn [db _]
   (-> db
       (assoc :running? true)
       (set-interval))))

(rf/reg-event-db
 :pause
 (fn [db _]
   (-> db
       (assoc :running? false)
       (stop-interval))))

(rf/reg-event-db
 :reset
 (fn [db _]
   (-> db
       (assoc :running? false)
       (assoc :passed-time 0)
       (stop-interval))))

(rf/reg-event-db
 :tick
 (fn [db _]
   (let [last-tick (:last-tick db)
         now       (timestamp)
         diff      (if last-tick
                     (- now last-tick)
                     0)]
     (-> db
         (update :passed-time + diff)
         (assoc :last-tick now)))))

                                        ; views
(defonce aquafit-blue "#008ca0")
(defonce machtfit-green "#34b78f")
(defonce sonnengruss-yellow "fbd872")
(defonce raspberrysmoothie-pink "e62e73")
(defonce night-black "#122020")
(defonce zen-white "#ffffff")

(defonce alarm-red "#d22")

(defn integer-field [key]
  [:input {:type      "input"
           :value     @(rf/subscribe [:get key])
           :on-change (fn [e] (rf/dispatch [:set key (int (-> e .-target .-value))]))}])


(defn clock []
  (let [duration        @(rf/subscribe [:get :duration])
        passed-time     @(rf/subscribe [:get :passed-time])
        remaining-time  (- duration passed-time)
        progress        (min 1.0 (/ passed-time duration))
        color           (condp < progress
                          0.98 alarm-red
                          0.9  raspberrysmoothie-pink
                          0.5  sonnengruss-yellow
                          machtfit-green)
        mins            (int (/ (Math/abs remaining-time) 60))
        secs            (int (rem (Math/abs remaining-time) 60))
        remaining-time-str (when remaining-time
                          (str (when (neg? remaining-time) "-") (format "%d:%02d" mins secs)))
        radius          49
        angle           (* 359.9999 progress)
        angle-rad       (* (- angle 90) (/ Math/PI 180.0))
        arc-x           (* radius (Math/cos angle-rad))
        arc-y           (* radius (Math/sin angle-rad))
        large-arc-flag  (if (> angle 180) 1 0)]
    [:g {:transform "translate(50, 50)"}
     [:circle {:cx    0
               :cy    0
               :r     radius
               :style {:stroke "none"
                       :fill   aquafit-blue}}]
     (when passed-time
       [:path {:d     (s/join " " (map str ["M" 0 (- radius) "A" radius radius 0 large-arc-flag 1 arc-x arc-y "L" 0 0 "Z"]))
               :style {:stroke     "none"
                       :fill       color
                       :transition "fill 1s"}}])
     (when remaining-time
       [:text {:x           (- (* 6.5 (count remaining-time-str)))
               :y           6
               :text-anchor "start"
               :style       {:fill      zen-white
                             :stroke    "none"
                             :font-size 25}}
        remaining-time-str])]))

(defn app []
  (fn []
    [:div
     {:style {:width "100%"}}
     [:div
      "duration" [integer-field :duration]
      [:button {:on-click #(rf/dispatch [:resume])} "resume"]
      [:button {:on-click #(rf/dispatch [:pause])} "pause"]
      [:button {:on-click #(rf/dispatch [:reset])} "reset"]]
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
