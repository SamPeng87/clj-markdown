(ns clj-markdown.funcs
  (:import (clojure.lang PersistentHashMap PersistentArrayMap)))

(defn replaceRegex
  [regexSource & replaceTarget]
  (let [replaceData (apply list replaceTarget)
        replaceResult (reduce (fn
                                [sourceString [name value]]
                                (clojure.string/replace sourceString name value))
                              regexSource replaceData)]
    replaceResult))

(defn replaceHashMap
  [source key target]
  (assoc source key target))

(defn regexMathesCI
  [string pattern]
  (.matches (re-matcher (re-pattern (. java.util.regex.Pattern compile
                                       pattern java.util.regex.Pattern/CASE_INSENSITIVE))
                        string)))

(defn escape
  ([^String string]
   (escape string false))
  ([string ^Boolean encode]
   (clojure.string/escape (if encode
                            (.replaceAll string "&" "&amp;")
                            (.replaceAll string "&(?!#?\\w+;)" "&amp;")) {
                                                                          "<" "&lt;"
                                                                          ">" "&gt;"
                                                                          "\"" "&quot;"
                                                                          "'"  "&#39"
                                                                          })))
(defn checkMethodPattern
  [method string status]
  (let [metaData (meta method)
        statusTemp (for [[k v] (select-keys (meta method) (keys status))]
                         (if (status k)
                           (or (= v 1) (= v -1))
                           (or (= v 0) (= v -1))))
        statusCheck (if (empty? statusTemp)
                      true
                      (reduce #(and % %2) statusTemp))]
    (if statusCheck
      (let [
            pattern (metaData :pattern)
            cap (re-find (re-matcher pattern string))]
        (if (nil? cap)
          false
          (if (vector? cap)
            cap
            (vector cap))))
      false)))

(defn executeInputByCheck
  [string cap]
  (subs string (count (first cap))))


(defn defSwitchCheck
  "循环执行funcs中的方法，每个方法执行check检查。
  如果检查通过，将check检查的结果给方法来执行，这个参数在最后一位"
  [p check status executeInputByCheck funcs]
  (loop [p p , funcs funcs]
    (if funcs
      (let [func (first funcs)
            statusTemp (reduce #(into % %2) (for [[k v] status]
                                              [k v]))
            test (check func p status)]
        (if test
          [(executeInputByCheck p test) (apply func (into [test] statusTemp))]
          (recur p (next funcs)))))))
(defn whileTranslateString
  ([src status translateFuncs executeInputByCheck check resultJoin]
   (whileTranslateString src status nil translateFuncs executeInputByCheck check resultJoin))
  ([src status changeStatusResult translateFuncs executeInputByCheck check resultJoin]
   (loop [src src, result (resultJoin),status status]
     (if (= src "")
       result
       (let [[rSrc rResult] (defSwitchCheck
                              src
                              check
                              status
                              executeInputByCheck
                              translateFuncs)
             [rSrc rResult status] (if (or (nil? changeStatusResult)
                                           (not (= (type rResult) PersistentArrayMap)))
                       [rSrc rResult status]
                       (changeStatusResult rSrc rResult status))
             rResult (resultJoin result rResult)]
         (println status)
         (recur rSrc rResult status)))
     )))

