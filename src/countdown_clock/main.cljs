(ns countdown-clock.main
  (:require [clojure.string :as s]
            [goog.string :as gstring]
            [goog.string.format]  ;; required for release build
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

(rf/reg-event-db
 :add-to-total-duration
 (fn [db [_ value]]
   (update db :duration + value)))

(rf/reg-event-db
 :toggle-controls
 (fn [db _]
   (update db :hide-controls? not)))

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
 :toggle
 (fn [db _]
   (if (:running? db)
     (rf/dispatch [:pause])
     (rf/dispatch [:resume]))
   db))

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
(defonce sonnengruss-yellow "#fbd872")
(defonce raspberrysmoothie-pink "#e62e73")
(defonce night-black "#122020")
(defonce zen-white "#ffffff")
(defonce alarm-red "#d22")

(defn get-string-width [text font-size]
  (let [element (js/document.getElementById "text-test")]
    (set! (.. element -style -fontSize) font-size)
    (set! (.. element -innerHTML) text)
    (inc (.. element -clientWidth))))

(defn get-max-width [text font-size]
  (get-string-width (s/replace text #"[0-9]" "0") font-size))

(defn integer-field [key]
  [:input {:type      "input"
           :value     @(rf/subscribe [:get key])
           :on-change (fn [e] (rf/dispatch [:set key (int (-> e .-target .-value))]))}])

(defn clock []
  (let [duration                 @(rf/subscribe [:get :duration])
        passed-time              @(rf/subscribe [:get :passed-time])
        remaining-time           (js/Math.ceil (- duration passed-time))
        progress-duration        (max 0 duration)
        progress                 (min 1.0 (if (zero? progress-duration) 1.0 (/ passed-time progress-duration)))
        color                    (condp < progress
                                   0.98 alarm-red
                                   0.9  raspberrysmoothie-pink
                                   0.5  sonnengruss-yellow
                                   machtfit-green)
        mins                     (int (/ (Math/abs remaining-time) 60))
        secs                     (int (rem (Math/abs remaining-time) 60))
        font-size                25
        remaining-time-str-left  (when remaining-time
                                   (str (when (neg? remaining-time) "-") (gstring/format "%d" mins)))
        remaining-time-str-right (when remaining-time (gstring/format "%02d" secs))
        left-width               (get-max-width remaining-time-str-left font-size)
        right-width              (get-max-width remaining-time-str-right font-size)
        total-width              (get-max-width (str remaining-time-str-left ":" remaining-time-str-right) font-size)
        colon-width              (get-string-width ":" font-size)
        middle-width             (- total-width left-width right-width)
        colon-spacing            (inc (/ colon-width 2))
        colon-offset             (+ (- left-width (/ total-width  2))
                                    (/ middle-width 2))
        radius                   49
        angle                    (* 359.9999 progress)
        angle-rad                (* (- angle 90) (/ Math/PI 180.0))
        arc-x                    (* radius (Math/cos angle-rad))
        arc-y                    (* radius (Math/sin angle-rad))
        large-arc-flag           (if (> angle 180) 1 0)]
    [:g.no-select {:transform "translate(50, 50)"
                   :on-click  #(rf/dispatch [:toggle])
                   :style     {:cursor "pointer"}}
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
       [:g
        [:text {:x           (- colon-offset colon-spacing)
                :y           6
                :text-anchor "end"
                :style       {:fill      zen-white
                              :stroke    "none"
                              :font-size font-size}}
         remaining-time-str-left]
        [:text {:x           colon-offset
                :y           6
                :text-anchor "middle"
                :style       {:fill      zen-white
                              :stroke    "none"
                              :font-size font-size}}
         ":"]
        [:text {:x           (+ colon-offset colon-spacing)
                :y           6
                :text-anchor "start"
                :style       {:fill      zen-white
                              :stroke    "none"
                              :font-size font-size}}
         remaining-time-str-right] ]
       )]))

(defn adder-button [text duration]
  [:div.adder
   [:div.button.minus.no-select
    {:on-click #(rf/dispatch [:add-to-total-duration (- duration)])}
    "-"]
   [:div.button.text.no-select
    {:on-click #(do
                  (rf/dispatch [:set :passed-time 0])
                  (rf/dispatch [:set :duration duration]))}
    text]
   [:div.button.plus.no-select
    {:on-click #(rf/dispatch [:add-to-total-duration duration])}
    "+"]])

(defn icon [path]
  [:div {:style {:display        "inline-block"
                 :vertical-align "middle"
                 :height         "100%"}}
   [:img {:src   path
          :style {:height         "1em"
                  :vertical-align "middle"}}]])

(defn reset-button []
  [:div.button.reset.no-select
   {:on-click #(rf/dispatch [:reset])}
   (icon "img/refresh-icon.svg")])

(defn start-button []
  (let [running? @(rf/subscribe [:get :running?])]
    [:div.button.start.no-select
     {:on-click #(rf/dispatch [:toggle])}
     (if running?
       [:div.running (icon "img/pause-icon.svg")]
       [:div.paused (icon "img/play-icon.svg")])]))

(defn key-char [event]
  (.-key event))

(defn on-key-press [event]
  (let [key-char (key-char event)]
    (cond
      (= key-char " ")                   (rf/dispatch [:toggle])
      (= key-char "r")                   (rf/dispatch [:reset])
      (= key-char "h")                   (rf/dispatch [:toggle-controls])
      (= key-char "0")                   (rf/dispatch [:set :duration 0])
      (s/includes? "123456789" key-char) (rf/dispatch [:add-to-total-duration (* (js/parseInt key-char) 60)])
      :else                              (println key-char event)
      )))

(defn app []
  (set! (.-onkeypress js/window) on-key-press)
  (fn []
    (let [hide-controls? @(rf/subscribe [:get :hide-controls?])
          controls-style {:transition "opacity 1s"
                          :opacity    (if hide-controls? 0 1)}]
      [:div
       {:style {:width "100%"}}
       [:svg {:style               {:width  "100%"
                                    :height "97vh"}
              :viewBox             "0 0 100 100"
              :preserveAspectRatio "xMidYMid meet"}
        [clock]]
       [:div.buttons {:style controls-style}
        [adder-button "10s" 10]
        [adder-button "1m" 60]
        [adder-button "5m" 300]]
       [:div.buttons2 {:style controls-style}
        [reset-button]
        [start-button]]])))

(defn stop []
  (println "Stopping..."))

(defn start
  []
  (rf/dispatch-sync [:initialize-db])
  (r/render [app]
            (.getElementById js/document "app")))

(defn ^:export init []
  (start))
