(ns machtfit.meeting-timer.catmullrom
  (:require [clojure.string :as string]))

;; catmullrom

(defn smooth-component [dir v0 v1 v2 tension]
  (dir v1 (* tension (/ (- v2 v0) 6))))

(defn smooth-point [dir p0 p1 p2 tension]
  (->> (range 2)
       (map (fn [i] (smooth-component dir (p0 i) (p1 i) (p2 i) tension)))
       (vec)))

(defn calculate-cubic-bezier-curve
  [tension [p0 p1 p2 p3]]
  (let [cp1 (smooth-point + p0 p1 p2 tension)
        cp2 (smooth-point - p1 p2 p3 tension)]
    [p1 cp1 cp2 p2]))

(defn catmullrom
  [points & {:keys [tension closed?] :or {tension 1}}]
  (->> (concat [(if closed?
                  (last (drop-last points))
                  (first points))] points [(if closed?
                                             (second points)
                                             (last points))])
       (map (juxt :x :y))
       (partition 4 1)
       (map (partial calculate-cubic-bezier-curve tension))
       vec))

;; svg

(defn svg-move-to [[x y]]
  (str "M" x "," y))

(defn svg-curve-to [[_ cp1 cp2 p2]]
  (str "C" (string/join "," (flatten [cp1 cp2 p2]))))

(defn curve->svg-path [curve]
  (let [start (first (first curve))]
    (string/join "" (concat [(svg-move-to start)]
                            (map svg-curve-to curve)))))

(defn curve->svg-closed-path [curve]
  (str (curve->svg-path curve) "Z"))

;; position on path

(defn square [x]
  (* x x))

(defn distance [{x0 :x y0 :y} {x1 :x y1 :y}]
  (Math/sqrt (+ (square (- x0 x1))
                (square (- y0 y1)))))

(defn path->leg-lengths [path]
  (->> path
       (partition 2 1)
       (map (partial apply distance))))

(defn find-leg-progress [leg-lengths progress]
  (let [total-length      (apply + leg-lengths)
        adjusted-progress (* progress total-length)]
    (->> leg-lengths
         (map-indexed vector)
         (reduce
          (fn [[_ _ traversed] [index leg-length]]
            (let [next-traversed (+ traversed leg-length)]
              (if (> adjusted-progress next-traversed)
                [index 1 next-traversed]
                (reduced [index
                          (/ (- adjusted-progress traversed) leg-length)
                          adjusted-progress]))))
          [0 0 0]))))

(defn interpolate-point-cubic [[p1 cp1 cp2 p2] t]
  (let [t2  (* t t)
        t3  (* t2 t)
        tr  (- 1 t)
        tr2 (* tr tr)
        tr3 (* tr2 tr)]
    (->> (range 2)
         (map (fn [i] (+ (* tr3 (p1 i))
                         (* 3 tr2 t (cp1 i))
                         (* 3 tr t2 (cp2 i))
                         (* t3 (p2 i)))))
         (map vector [:x :y])
         (into {}))))

(defn cubic-bezier-length [bezier-segment]
  (let [steps  10
        ts     (map #(/ % steps) (range (inc steps)))
        points (map (partial interpolate-point-cubic bezier-segment) ts)]
    (->> points
         (partition 2 1)
         (map (partial apply distance))
         (reduce +))))

(defn curve->leg-lengths [path]
  (->> path
       (map cubic-bezier-length)))

(defn point-on-curve [path-data leg-lengths progress]
  (let [[leg-index sub-progress _] (find-leg-progress leg-lengths progress)]
    (interpolate-point-cubic (nth path-data leg-index) sub-progress)))

(defn partial-bezier-curve [bezier-segment start end]
  (let [steps  10
        ts     (map #(+ start (* (/ % steps) (- end start))) (range (inc steps)))
        points (map (partial interpolate-point-cubic bezier-segment) ts)]
    (catmullrom points)))

(defn partial-curve [curve start end]
  (let [leg-lengths                            (curve->leg-lengths curve)
        [start-leg-index start-sub-progress _] (find-leg-progress leg-lengths start)
        [end-leg-index end-sub-progress _]     (find-leg-progress leg-lengths end)]
    (if (>= end 1)
      curve
      (-> []
          (cond->
              (pos? start-sub-progress) (concat
                                         (partial-bezier-curve
                                          (nth curve start-leg-index)
                                          start-sub-progress
                                          1)
                                         (subvec curve (inc start-leg-index) end-leg-index))
              (zero? start-sub-progress) (concat
                                          (subvec curve start-leg-index end-leg-index))
              (= end-sub-progress 1)     (concat
                                          [(last curve)])
              (< end-sub-progress 1)     (concat
                                          (partial-bezier-curve
                                           (nth curve end-leg-index)
                                           0
                                           end-sub-progress)))
          vec))))
