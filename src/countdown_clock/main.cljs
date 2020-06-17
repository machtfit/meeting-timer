(ns countdown-clock.main
  (:require [clojure.string :as s]
            [countdown-clock.catmullrom :as cmr]
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

(rf/reg-event-db
 :toggle-help
 (fn [db _]
   (update db :show-help? not)))

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

(defn figure-eight [r amplitude speed]
  (let [t (* r speed)]
    [(* amplitude (Math/cos t))
     (* amplitude (Math/cos t) (Math/sin t))]))

(defn arc-point [radius remaining-time angle]
  (let [angle-rad           (* (- angle 90) (/ Math/PI 180.0))
        overtime-max        -30
        clamped-time        (max remaining-time overtime-max)
        reshape?            (neg? remaining-time)
        amplitude           (* 10 (/ clamped-time overtime-max) (Math/sin remaining-time))
        speed               (* 5 (/ clamped-time overtime-max))
        jerk-amplitude      (* 5 (/ clamped-time overtime-max))
        jerk-speed          (* 7 (/ clamped-time overtime-max))
        effective-radius    (if reshape?
                              (+ radius (* amplitude (Math/sin (+ (* 7 angle-rad) (* speed remaining-time)))))
                              radius)
        [offset-x offset-y] (if reshape?
                              (figure-eight remaining-time jerk-amplitude jerk-speed)
                              [0 0])]
    {:x     (+ (* effective-radius (Math/cos angle-rad)) offset-x)
     :y     (+ (* effective-radius (Math/sin angle-rad)) offset-y)
     :angle angle-rad
     :d     1}))

(defn clock []
  (let [duration                 @(rf/subscribe [:get :duration])
        passed-time              @(rf/subscribe [:get :passed-time])
        remaining-time-precise   (- duration passed-time)
        remaining-time           (js/Math.ceil remaining-time-precise)
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
        radius                   40
        angle                    (* 360 progress)
        shape-function           (partial arc-point radius (- remaining-time-precise 5))
        clock-shape-points       (vec (map shape-function (range 0 360.0001 1)))
        clock-shape              (cmr/catmullrom clock-shape-points)
        arc-points               (vec (map shape-function (concat (range 0 angle 1) [angle])))
        arc-curve                (cmr/catmullrom arc-points)]
    [:g.no-select {:transform "translate(50, 50)"
                   :on-click  #(rf/dispatch [:toggle])
                   :style     {:cursor "pointer"}}
     [:path {:d     (cmr/curve->svg-closed-path clock-shape)
             :style {:fill   aquafit-blue
                     :stroke "none"}}]
     (when (and passed-time (> (count arc-points) 1))
       [:path {:d     (s/join " " (map str ["M" 0 (- radius) (cmr/curve->svg-path arc-curve) "L" 0 0 "Z"]))
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
         remaining-time-str-right]])]))

(defn adder-button [text duration]
  [:div.adder
   [:div.button.minus.no-select
    {:on-click #(rf/dispatch [:add-to-total-duration (- duration)])
     :title    (str "Minus " text)}
    "-"]
   [:div.button.text.no-select
    {:on-click #(do
                  (rf/dispatch [:set :passed-time 0])
                  (rf/dispatch [:set :duration duration]))
     :title    (str "Set " text)}
    text]
   [:div.button.plus.no-select
    {:on-click #(rf/dispatch [:add-to-total-duration duration])
     :title    (str "Plus " text)}
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
   {:on-click #(rf/dispatch [:reset])
    :title    "Reset"}
   (icon "img/refresh-icon.svg")])

(defn start-button []
  (let [running? @(rf/subscribe [:get :running?])]
    [:div.button.start.no-select
     {:on-click #(rf/dispatch [:toggle])
      :title    (if running? "Pause" "Start")}
     (if running?
       [:div.running (icon "img/pause-icon.svg")]
       [:div.paused (icon "img/play-icon.svg")])]))

(defn key-display [char]
  (let [space-key? (= char " ")]
    [:div.key.no-select {:class (when space-key? "space-key")}
     [:img {:src (if space-key? "img/space-key.svg" "img/key.svg")}]
     [:div char]]))

(defn help []
  (let [keys       [["?" "Show/hide this help"]
                    ["r" "Reset timer to start time"]
                    ["h" "Show/hide controls"]
                    [" " "Start/pause timer"]
                    ["0" "Set start time to 00:00"]]
        show-help? @(rf/subscribe [:get :show-help?])]
    [:div.help {:style {:transition "opacity 1s"
                        :opacity    (if show-help? 1 0)}}
     [:div.table
      (for [[key description] keys]
        ^{:key key}
        [:div.row
         [:div.cell.key-cell [key-display key]]
         [:div.cell.key-description-cell description]])
      [:div.row
       [:div.cell.key-cell
        [key-display "1"]
        [:div.key [:div "to"]]
        [key-display "9"]]
       [:div.cell.key-description-cell "Add <N> minutes"]]]]))

(defn key-char [event]
  (.-key event))

(defn on-key-press [event]
  (let [key-char (key-char event)]
    (cond
      (= key-char " ")                   (rf/dispatch [:toggle])
      (= key-char "r")                   (rf/dispatch [:reset])
      (= key-char "h")                   (rf/dispatch [:toggle-controls])
      (= key-char "?")                   (rf/dispatch [:toggle-help])
      (= key-char "0")                   (rf/dispatch [:set :duration 0])
      (s/includes? "123456789" key-char) (rf/dispatch [:add-to-total-duration (* (js/parseInt key-char) 60)])
      :else                              (println key-char event))))

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
        [adder-button "5m" 300]
        [reset-button]
        [start-button]]
       [:div.logo
        [:a {:href   "https://www.machtfit.de/"
             :target "_blank"}
         [:div "Powered by"]
         [:img {:src "img/machtfit-logo.png"}]]]
       [:div {:style controls-style}
        [:div {:on-click #(rf/dispatch [:toggle-help])
               :style    {:position "absolute"
                          :top      "1em"
                          :left     "1em"
                          :cursor   "pointer"}}
         [key-display "?"]]
        [help]]])))

(defn stop []
  (println "Stopping..."))

(defn start
  []
  (rf/dispatch-sync [:initialize-db])
  (r/render [app]
            (.getElementById js/document "app")))

(defn ^:export init []
  (start))
