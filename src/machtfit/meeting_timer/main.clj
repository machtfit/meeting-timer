(ns machtfit.meeting-timer.main
  (:require [clojure.string :as string]))

(defmacro r-sub
  [sub-name args inputs body]
  (let [query-v             (into ['_] args)
        partitioned-inputs  (partition 2 inputs)
        input-query-vectors (mapv second partitioned-inputs)
        input-names*        (mapv first partitioned-inputs)
        input-names         (if (empty? input-names*) 'db input-names*)
        ns-name             (str *ns*)
        ns-prefix           (str (subs ns-name 0 (string/index-of ns-name \.)) "/")
        input-fn            `(fn [~query-v ~'_]
                               [~@(for [input-query-vector input-query-vectors]
                                    (if (vector? input-query-vector)
                                      (let [[input-sub-name & input-sub-args] input-query-vector]
                                        (if (and (keyword? input-sub-name) (string/index-of (name input-sub-name) "/"))
                                          `(re-frame.core/subscribe ~input-query-vector)
                                          `(re-frame.core/subscribe [(keyword (str ~ns-prefix (name ~input-sub-name))) ~@input-sub-args])))
                                      `(re-frame.core/subscribe ~input-query-vector)))])
        value-fn            `(fn [~input-names ~query-v ~'_] ~body)
        sub-name*           `(keyword (str ~ns-prefix (name ~sub-name)))]
    (if (empty? input-query-vectors)
      `(re-frame.core/reg-sub ~sub-name* ~value-fn)
      `(re-frame.core/reg-sub ~sub-name* ~input-fn ~value-fn))))
