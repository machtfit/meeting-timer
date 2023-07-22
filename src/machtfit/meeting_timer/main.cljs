(ns machtfit.meeting-timer.main
  (:require [clojure.string :as s]
            [goog.string :as gstring]
            [goog.string.format] ;; required for release build
            [machtfit.meeting-timer.catmullrom :as cmr]
            [re-frame.core :as rf]
            [re-frame.subs :as r-subs]
            [reagent.dom.client :as r]))

(defonce aquafit-blue "#008ca0")
(defonce machtfit-green "#34b78f")
(defonce sonnengruss-yellow "#fbd872")
(defonce raspberrysmoothie-pink "#e62e73")
(defonce night-black "#122020")
(defonce zen-white "#ffffff")
(defonce alarm-red "#d22")

(def font-size 25)
(def ticks-per-second 20)

(def colon-width 5)
(def digit-width 11)

(def radius 47)

(defn timestamp []
  (/ (.now js/Date) 1000.0))

(rf/reg-sub :get
  (fn [db [_ key]]
    (key db)))

(rf/reg-sub :remaining-time
  :<- [:get :duration]
  :<- [:get :passed-time]

  (fn [[duration passed-time] _]
    (- duration passed-time)))

(rf/reg-sub :remaining-seconds
  :<- [:remaining-time]

  (fn [remaining-time _]
    (some-> remaining-time Math/ceil)))

(rf/reg-sub :remaining-time-string-parts
  :<- [:remaining-seconds]

  (fn [remaining-seconds _]
    (when remaining-seconds
      (let [absolute-remaining-seconds (Math/abs remaining-seconds)
            mins (int (/ absolute-remaining-seconds 60))
            secs (int (rem absolute-remaining-seconds 60))
            left-digits (gstring/format "%d" mins)
            left (str (when (neg? remaining-seconds)
                        "-")
                      left-digits)
            right (gstring/format "%02d" secs)]
        [left right]))))

(rf/reg-sub :show-clock-base-shape?
  :<- [:progress]

  (fn [progress _]
    (< progress 1)))

(rf/reg-sub :show-clock-progress-shape?
  :<- [:progress]

  (fn [progress _]
    (pos? progress)))

(rf/reg-sub :colon-spacing-data
  :<- [:remaining-time-string-parts]

  (fn [[time-left _] _]
    (let [left-width (* digit-width (count time-left))
          right-width (* digit-width 2)
          middle-width colon-width
          total-width (+ left-width
                         middle-width
                         right-width)
          colon-spacing (inc (/ colon-width 2))
          colon-offset (+ (- left-width (/ total-width 2))
                          (/ middle-width 2))]
      [colon-offset colon-spacing])))

(rf/reg-sub :progress
  :<- [:get :duration]
  :<- [:get :passed-time]

  (fn [[duration passed-time] _]
    (let [progress-duration (max 0 duration)]
      (min 1.0 (if (zero? progress-duration)
                 1.0
                 (/ passed-time progress-duration))))))

(rf/reg-sub :progress-color
  :<- [:progress]

  (fn [progress _]
    (condp < progress
      0.98 alarm-red
      0.9 raspberrysmoothie-pink
      0.5 sonnengruss-yellow
      machtfit-green)))

(rf/reg-event-db :initialize-db
  (fn [db [_ initial-time]]
    (when-let [profile-timer (:profile-timer db)]
      (js/clearInterval profile-timer))
    (merge {:duration (or initial-time 300)
            :running? false}
           db)))

(rf/reg-event-db :set
  (fn [db [_ key value]]
    (assoc db key value)))

(defn change-url [duration]
  (let [duration-str (if (zero? (mod duration 60))
                       (str (/ duration 60) "m")
                       (str duration "s"))]
    (js/history.pushState #js{} nil (str ".?t=" duration-str))))

(rf/reg-event-db :set-duration
  (fn [db [_ value]]
    (let [new-db (assoc db :duration value)]
      (change-url (:duration new-db))
      new-db)))

(rf/reg-event-db :add-to-total-duration
  (fn [db [_ value]]
    (let [new-db (update db :duration + value)]
      (change-url (:duration new-db))
      new-db)))

(rf/reg-event-db :toggle-controls
  (fn [db _]
    (let [new-db (update db :hide-controls? not)]
      (if (:hide-controls? new-db)
        (assoc new-db :show-help? false)
        new-db))))

(rf/reg-event-db :toggle-help
  (fn [db _]
    (update db :show-help? not)))

(defn stop-interval [db]
  (let [previous-timer (:interval-timer db)]
    (when previous-timer
      (js/clearInterval previous-timer))
    (dissoc db :interval-timer)))

(defn set-interval [db]
  (-> db
      (stop-interval)
      (assoc :last-tick (timestamp))
      (assoc :interval-timer (js/setInterval #(rf/dispatch [:tick]) (/ 1000 ticks-per-second)))))

(rf/reg-event-db :resume
  (fn [db _]
    (-> db
        (assoc :running? true)
        (set-interval))))

(rf/reg-event-db :pause
  (fn [db _]
    (-> db
        (assoc :running? false)
        (stop-interval))))

(rf/reg-event-fx :toggle
  (fn [{:keys [db]} _]
    {:dispatch (if (:running? db)
                 [:pause]
                 [:resume])}))

(rf/reg-event-db :reset
  (fn [db _]
    (-> db
        (assoc :running? false)
        (assoc :passed-time 0)
        (stop-interval))))

(rf/reg-event-db :tick
  (fn [db _]
    (let [last-tick (:last-tick db)
          now (timestamp)
          diff (if last-tick
                 (- now last-tick)
                 0)]
      (-> db
          (update :passed-time + diff)
          (assoc :last-tick now)))))

(defn wobble-position [r amplitude speed]
  (let [t (* r speed)]
    [(* amplitude (Math/cos (* 0.8 t)))
     (* amplitude (Math/sin (* 0.7 t)))]))

(defn arc-point-wobble [remaining-time angle]
  (let [angle-rad (* (- angle 90) (/ Math/PI 180.0))
        overtime-max -30
        clamped-time (max remaining-time overtime-max)
        reshape? (neg? remaining-time)
        amplitude (* 10 (/ clamped-time overtime-max) (Math/sin remaining-time))
        speed (* 5 (/ clamped-time overtime-max))
        jerk-amplitude (* 5 (/ clamped-time overtime-max))
        jerk-speed (* 7 (/ clamped-time overtime-max))
        effective-radius (if reshape?
                           (+ radius (* amplitude (Math/sin (+ (* 7 angle-rad) (* speed remaining-time)))))
                           radius)
        [offset-x offset-y] (if reshape?
                              (wobble-position remaining-time jerk-amplitude jerk-speed)
                              [0 0])]
    {:x (+ (* effective-radius (Math/cos angle-rad)) offset-x)
     :y (+ (* effective-radius (Math/sin angle-rad)) offset-y)}))

(defn arc-point-default [angle]
  (let [angle-rad (* (- angle 90) (/ Math/PI 180.0))]
    {:x (* radius (Math/cos angle-rad))
     :y (* radius (Math/sin angle-rad))}))

(defn time-string []
  (when-let [[time-left time-right] @(rf/subscribe [:remaining-time-string-parts])]
    (let [[colon-offset colon-spacing] @(rf/subscribe [:colon-spacing-data])]
      [:g
       [:text {:x (- colon-offset colon-spacing)
               :y 6
               :text-anchor "end"
               :style {:fill zen-white
                       :stroke "none"
                       :font-size font-size}}
        time-left]
       [:text {:x colon-offset
               :y 6
               :text-anchor "middle"
               :style {:fill zen-white
                       :stroke "none"
                       :font-size font-size}}
        ":"]
       [:text {:x (+ colon-offset colon-spacing)
               :y 6
               :text-anchor "start"
               :style {:fill zen-white
                       :stroke "none"
                       :font-size font-size}}
        time-right]])))

(rf/reg-sub :clock-base-shape-arc-function
  :<- [:remaining-time]

  (fn [remaining-time _]
    (let [wobble-time (- remaining-time 5)]
      (if (neg? wobble-time)
        (partial arc-point-wobble wobble-time)
        arc-point-default))))

(rf/reg-sub :clock-base-shape
  :<- [:clock-base-shape-arc-function]

  (fn [shape-function _]
    (let [clock-shape-points (mapv shape-function (range 0 361 20))]
      (cmr/catmullrom clock-shape-points :closed? true))))

(rf/reg-sub :clock-base-path
  :<- [:clock-base-shape]

  (fn [clock-base-shape _]
    (cmr/curve->svg-closed-path clock-base-shape)))

(defn clock-base-shape []
  (when @(rf/subscribe [:show-clock-base-shape?])
    [:path {:d @(rf/subscribe [:clock-base-path])
            :style {:fill aquafit-blue
                    :stroke "none"}}]))

(rf/reg-sub :clock-progress-shape
  :<- [:clock-base-shape]
  :<- [:progress]

  (fn [[clock-shape progress] _]
    (cmr/partial-curve clock-shape 0 progress)))

(rf/reg-sub :clock-progress-path
  :<- [:clock-progress-shape]

  (fn [clock-progress-shape _]
    (s/join " " (map str [(cmr/curve->svg-path clock-progress-shape) "L" 0 0 "Z"]))))

(defn clock-progress-shape []
  (when @(rf/subscribe [:show-clock-progress-shape?])
    [:path.transition-fill {:d @(rf/subscribe [:clock-progress-path])
                            :style {:stroke "none"
                                    :fill @(rf/subscribe [:progress-color])}}]))

(defn clock []
  [:svg {:style {:width "100%"
                 :height "97vh"}
         :viewBox "0 0 100 100"
         :preserveAspectRatio "xMidYMid meet"}
   [:g.no-select {:transform "translate(50, 50)"
                  :on-click #(rf/dispatch [:toggle])
                  :style {:cursor "pointer"}}
    [clock-base-shape]
    [clock-progress-shape]
    [time-string]]])

(defn adder-button [text duration]
  [:div.adder
   [:div.button.minus.no-select
    {:on-click #(rf/dispatch [:add-to-total-duration (- duration)])
     :title (str "Minus " text)}
    "-"]
   [:div.button.text.no-select
    {:on-click #(do
                  (rf/dispatch [:set :passed-time 0])
                  (rf/dispatch [:set-duration duration]))
     :title (str "Set " text)}
    text]
   [:div.button.plus.no-select
    {:on-click #(rf/dispatch [:add-to-total-duration duration])
     :title (str "Plus " text)}
    "+"]])

(defn icon [path]
  [:div {:style {:position "relative"
                 :height "100%"}}
   [:img {:src path
          :style {:position "absolute"
                  :top "50%"
                  :left "50%"
                  :height "32px"
                  :transform "translate(-50%, -50%)"}}]])

(defn reset-button []
  [:div.button.reset.no-select
   {:on-click #(rf/dispatch [:reset])
    :title "Reset"}
   (icon "img/refresh-icon.svg")])

(defn start-button []
  (let [running? @(rf/subscribe [:get :running?])]
    [:div.button.start.no-select
     {:on-click #(rf/dispatch [:toggle])
      :title (if running?
               "Pause"
               "Start")}
     (if running?
       (icon "img/pause-icon.svg")
       (icon "img/play-icon.svg"))]))

(defn key-display [char]
  (let [space-key? (= char " ")]
    [:div.key.no-select {:class (when space-key?
                                  "space-key")}
     [:img {:src (if space-key?
                   "img/space-key.svg"
                   "img/key.svg")}]
     [:div char]]))

(defn controls-style []
  (let [hide-controls? @(rf/subscribe [:get :hide-controls?])]
    {:opacity (if hide-controls?
                0
                1)
     :pointer-events (when hide-controls?
                       "none")}))

(defn help []
  (let [keys [["?" "Show/hide this help"]
              ["r" "Reset timer to start time"]
              ["h" "Show/hide controls"]
              [" " "Start/pause timer"]
              ["0" "Set start time to 0:00"]]
        show-help? @(rf/subscribe [:get :show-help?])]
    [:<>
     [:div.transition-opacity {:on-click #(rf/dispatch [:toggle-help])
                               :style (merge
                                       {:position "absolute"
                                        :top "20px"
                                        :left "20px"
                                        :cursor "pointer"}
                                       (controls-style))}
      [key-display "?"]]
     [:div.help.no-select {:style {:transition "opacity 1s"
                                   :opacity (if show-help?
                                              1
                                              0)
                                   :pointer-events (when-not show-help?
                                                     "none")}}
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
        [:div.cell.key-description-cell "Add <N> minutes"]]
       [:div.row
        [:div.cell.last {:style {:padding-top "20px"}}
         [:a {:href "https://github.com/machtfit/meeting-timer/"
              :target "_blank"
              :title "meeting-timer on GitHub"
              :style {:cursor "pointer"}}
          [:img {:src "img/github-logo.svg"
                 :style {:width "40px"
                         :opacity 0.8}}]]]
        [:div.cell.last {:style {:padding-top "20px"}}
         "fork and code!"]]]]]))

(defn on-key-press [event]
  (let [key-char (.-key event)]
    (cond
      (= key-char " ") (rf/dispatch [:toggle])
      (= key-char "r") (rf/dispatch [:reset])
      (= key-char "h") (rf/dispatch [:toggle-controls])
      (= key-char "?") (rf/dispatch [:toggle-help])
      (= key-char "0") (rf/dispatch [:set-duration 0])
      (s/includes? "123456789" key-char) (rf/dispatch [:add-to-total-duration (* (js/parseInt key-char) 60)]))))

(defn controls []
  [:div.transition-opacity
   {:style (controls-style)}
   [:div.buttons
    [adder-button "10s" 10]
    [adder-button "1m" 60]
    [adder-button "5m" 300]
    [reset-button]
    [start-button]]])

(defn logo []
  [:div.logo
   [:a {:href "https://www.machtfit.de/"
        :target "_blank"}
    [:img {:src "img/machtfit-logo.png"}]]])

(defn clock-and-controls []
  (let [hide-controls? @(rf/subscribe [:get :hide-controls?])
        controls-width (if hide-controls?
                         "0px"
                         "160px")]
    [:div {:style {:width "100%"}}
     [:div.transition-width
      {:style {:width (str "calc(100% - " controls-width ")")}}
      [clock]]
     [:div.transition-width
      {:style {:min-width controls-width
               :width controls-width}}
      [controls]]]))

(defn app []
  (set! (.-onkeypress js/window) on-key-press)
  (fn []
    [:div
     {:style {:width "100%"}}
     [clock-and-controls]
     [help]
     [logo]]))

(defn parse-time-str [time-str]
  (let [[_ number-str unit] (re-find #"(-?\d+)([ms]?)" (or time-str ""))
        number (js/parseInt number-str)]
    (case unit
      "" number
      "s" number
      "m" (* number 60)
      nil)))

(defn ^:export init []
  (r-subs/clear-subscription-cache!)
  (let [get-params (js/URLSearchParams. js/window.location.search)
        time-str (.get get-params "t")
        initial-time (parse-time-str time-str)]
    (rf/dispatch-sync [:initialize-db initial-time]))
  (let [root (r/create-root (.getElementById js/document "app"))]
    (r/render root [app])))
