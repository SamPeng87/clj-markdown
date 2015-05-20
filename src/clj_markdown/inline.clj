(ns clj-markdown.inline
  (:use clj-markdown.funcs)
  (:use clj-markdown.tags)
  (:import (clojure.lang PersistentHashMap)))


(def inlineBase {
                 :escape "^\\\\([\\\\`*{}\\[\\]()#+\\-.!_>])"
                 :autolink "^<([^ >]+(@|:\\/)[^ >]+)>"
                 :tag "^<!--[\\s\\S]*?-->|^<\\/?\\w+(?:\\\"[^\\\"]*\\\"|'[^']*'|[^'\\\">])*?>"
                 :link "^!?\\[(inside)\\]\\(href\\)"
                 :reflink "^!?\\[(inside)\\]\\s*\\[([^\\]]*)\\]"
                 :nolink "^!?\\[((?:\\[[^\\]]*\\]|[^\\[\\]])*)\\]"
                 :strong "^__([\\s\\S]+?)__(?!_)|^\\*\\*([\\s\\S]+?)\\*\\*(?!\\*)"
                 :em "^\\b_((?:__|[\\s\\S])+?)_\\b|^\\*((?:\\*\\*|[\\s\\S])+?)\\*(?!\\*)"
                 :code "^(`+)\\s*([\\s\\S]*?[^`])\\s*\\1(?!`)"
                 :br   "^ {2,}\n(?!\\s*$)"
                 :del ""
                 :url ""
                 :text "^[\\s\\S]+?(?=[\\\\<!\\[_*`]| {2,}\\n|$)"
                 :_inside "(?:\\[[^\\]]*\\]|[^\\[\\]]|\\](?=[^\\[]*\\]))*"
                 :_href   "\\s*<?([\\s\\S]*?)>?(?:\\s+['\\\"]([\\s\\S]*?)['\\\"])?\\s*"
                 })

(def inlineReplaced (-> inlineBase
                         (replaceHashMap :link (replaceRegex (inlineBase :link)
                                                             ["inside" (inlineBase :_inside)]
                                                             ["href" (inlineBase :_href)]))
                         (replaceHashMap :reflink (replaceRegex (inlineBase :reflink)
                                                             ["inside" (inlineBase :_inside)]))
                         ))
(def inlineGFM (-> inlineReplaced
                   (assoc :escape (replaceRegex (inlineReplaced :escape)
                                                ["])","~|])"]))
                   (assoc :url "^(https?:\\/\\/[^\\s<]+[^<.,:;\\\"')\\]\\s])")
                   (assoc :del "^~~(?=\\S)([\\s\\S]*?\\S)~~")
                   (assoc :text (replaceRegex (inlineReplaced :text)
                                              ["]|" "~]|"]
                                              ["|" "|https?://|"]))
                   ))
(def inlineBreaks (-> inlineGFM
                      (assoc :br (replaceRegex (inlineReplaced :br)
                                               ["{2,}" "*"]))
                      (assoc :text (replaceRegex (inlineGFM :text)
                                                 ["{2,}" "*"]))))
(declare inline)
(declare ^:dynamic links)
(defn- regex
  [name]
  (re-pattern (inlineGFM name)))

(defn- mangleLink
  [text]
  (let [l (count text)]
    (loop [i 0, out ""]
      (if (< i l)
        (let [ch (int (.charAt i))
              ch (if (> (rand) 0.5)
                   (str "x" (. Integer toHexString ch))
                   ch)]
          (recur (inc i) (str out "&#" + ch + ";")))
        out))))

(defn smartypants [text]
  ;这里是不是用配置控制一下
  text)

(defn- outputLink
  [cap link inLink]
  (let [
        tempCap (get cap 1)
        href (escape (link :href))
        title (if (nil? (link :title))
                nil
                (escape (link :title)))]
    (if (not= (.charAt (first cap) 0) "!")
      (a (inline tempCap inLink links) :href href :title title)
      (img tempCap :href href :link title))))

(defn- inlineEscape
  {
   :pattern (regex :escape)
   }
  [cap & status]
  (println "inlineEscape")
  (get cap 1))

(defn- inlineAutoLink
  {
   :pattern (regex :autolink)
   }
  [cap & status]
  (println "inlineAutoLink")
  (let [match (get cap 1)]
    (if (= (get cap 2) "@")
      (let [text (if (= (.charAt(6) match) ":")
                   (mangleLink (subs match 7))
                   (mangleLink match)
                   )
            href (str (mangleLink "mailto:") text)]
        (a text :href href))
      (let [text (escape match)
            href text]
        (a text :href href)))
    ))
(defn- inlineUrl
  {
   :pattern (regex :url)
   :inLink 0
   }
  [cap & status]
  (println "inlineUrl")
  (let [match (get cap 1)
        text (escape match)
        href text]
    (a text :href href)))

(defn- inlineTag
  {
   :pattern (regex :tag)
   }
  [cap & status]
  (println "inlineTag")
  (let [
        inLink ((apply hash-map status) :inLink)
        match (first cap)
        inLink (cond
                 (and (not inLink) (regexMathesCI match "^<a .+")) true
                 (and inLink (regexMathesCI match "^<\\/a>")) false
                 true inLink)]
    {:result (escape match) :status {:inLink inLink}}))

(defn- inlineLink
  {
   :pattern (regex :link)
   }
  [cap & status]
  (println "inlineLink")
  (outputLink cap {
                   :href (get cap 2)
                   :title (get cap 3)
                   } true))

(defn- inlineInnerLink
  [cap]
  (println cap)
  (println (get cap 1))
  (println (get cap 2))
  (let [
        link (.replaceAll (or (get cap 2) (get cap 1)) "\\s+" " ")
        link (get links (.toLowerCase link))]
    (if (or (nil? link) (nil? (.getHref link)))
      {:result (.charAt (first cap) 0)
       :src (.substring (first cap) 1)}
      (outputLink cap (.toHashMap link) true))))

(defn- inlineReflink
  {
   :pattern (regex :reflink)
   }
  [cap & status]
  (println "inlineRefLink")
  (inlineInnerLink cap))

(defn- inlineNolink
  {
   :pattern (regex :nolink)
   }
  [cap & status]
  (println "inlineNoLink")
  (inlineInnerLink cap))


(defn- inlineStrong
  {
   :pattern (regex :strong)
   }
  [cap & status]
  (println "inlineStrong")
  (let [status (apply hash-map status)]
    (strong (inline (or (get cap 2) (get cap 1)) links (status :inLink)))))


(defn- inlineEm
  {
   :pattern (regex :em)
   }
  [cap & status]
  (println "inlineEm")
  (let [status (apply hash-map status)]
    (em (inline (or (get cap 2) (get cap 1)) links (status :inLink)))))

(defn- inlineCode
  {
   :pattern (regex :code)
   }
  [cap & status]
  (println "inlineCode")
  (let [status (apply hash-map status)]
    (codespan (escape (get cap 2) true))))

(defn- inlineBr
  {
   :pattern (regex :br)
   :continue false
   }
  [cap & status]
  (br))

(defn- inlineDel
  {
   :pattern (regex :del)
   :continue false
   }
  [cap & status]
  (println "inlineDel")
  (let [status (apply hash-map status)]
    (del (inline (get cap 1) links (status :inLink)))))


(defn- inlineText
  {
   :pattern (regex :text)
   :continue false
   }
  [cap & status]
  (println "inlineText")
  (text (escape (smartypants (first cap)))))

(defn- resultJoin
  ([]
    "")
  ([source target]
   (str source target)))

(defn splitStatusFromResult
  [src result status]
  (if (contains? result :src)
    [(str (result :src) src) (result :result) (if (contains? result :status)
                                                (result :status)
                                                status)]
    [src (result :result) (result :status)]))

(defn inline
  [cap links inLink]
  (binding [links links]
    (let [inlineMethod
          [#'inlineEscape
           #'inlineAutoLink
           #'inlineUrl
           #'inlineTag
           #'inlineLink
           #'inlineReflink
           #'inlineNolink
           #'inlineStrong
           #'inlineEm
           #'inlineCode
           #'inlineBr
           #'inlineDel
           #'inlineText]]
      (whileTranslateString
        cap
        {:inLink inLink}
        splitStatusFromResult
        inlineMethod
        executeInputByCheck
        checkMethodPattern
        resultJoin))))
