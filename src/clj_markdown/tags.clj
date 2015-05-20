(ns clj-markdown.tags
  (:use clj-markdown.funcs))



(defprotocol Links
  (getName [this] "返回名字")
  (getHref [this] "返回链接")
  (getTitle [this] "返回标题")
  (toHashMap [this]))

(deftype NewLine [])
(deftype Code    [lines lang])
(deftype Heading [head depth])
(deftype Paragraph [body])
(deftype Hr [])
(deftype Blockquote [body])
(deftype Lists [order body])
(deftype Table     [headers aligns cells])
(deftype Html      [body pre])
(deftype Text [body])


(defrecord Link [href title name]
  Links
  (getName [this] name)
  (getHref [this] href)
  (getTitle [this] title)
  (toHashMap [this] {
                     :href href
                     :title title
                     :name name
                     }))
(defmacro param-attrs
  [attrs]
  `(let [attrsTemp# (hash-map ~@attrs)]
    (if (not-empty attrsTemp#)
                  (clojure.string/join (for[[k# v#] attrsTemp#]
                                         (str " " (if (keyword? k#)
                                                    (.substring (str k#) 1)
                                                    k#) "=\"" v# "\" ")))
                  "")))

(defmacro codespan
  [body & attrs]
  `(let [attrs# (param-attrs ~attrs)]
    (str "<code" attrs# ">" ~body "</code>")))


(defmacro code
  [code lang]
  `(if (not ~lang)
     (str "<pre>" (codespan ~code) "</pre>\n")
     (str "<pre>" (codespan ~code :lang ~lang) "</pre>\n")))

(defmacro blockquote
  [quote]
  `(str "<blockquote>\n"
        ~quote
        "</blockquote>\n"))

(defmacro html
  [body]
  body)

(defmacro h
  [level text]
  `(str "<h"
       ~level
       ">"
       ~text
       "</h" ~level ">\n"))
(defmacro hr
  []
  "<hr/>\n")

(defmacro ol
  [body]
  `(str "<ol>\n" ~body "</ol>\n"))

(defmacro ul
  [body]
  `(str "<ul>\n" ~body "</ul>\n"))

(defmacro li
  [text]
  `(str "<li>" ~text "</li>"))

(defmacro p
  [text]
   `(str "<p>"
        ~text
        "</p>\n"))

(defmacro table
  [header body]
  `(str "<table>"
       "<thead>"
       ~header
       "</thead>"
       "<tbody>"
       ~body
       "</tbody>"
       "</table>"))

(defmacro tablerow
  [content]
  `(str "<tr>" ~content "</tr>"))

(defmacro tablecell
  [content flags]
  `(let [type# (if (~flags :header)
               "th"
               "td")
        tag# (if (~flags :align)
              (str "<" type# " style=\"text-align:" (~flags :align) "\">")
              (str "<" type# ">"))]
    (str tag# ~content "</" type# ">")))

(defmacro text
  [body]
  body)

(defmacro strong
  [body]
  `(str "<b>" ~body "</b>"))

(defmacro em
  [body]
  `(str "<em>" ~body "</em>"))

(defmacro del
  [body]
  `(str "<del>" ~body "<del>"))
(defmacro br
  []
  "<br/>")

(defmacro a
  [name & attrs]
  `(let [attrs# (param-attrs ~attrs)]
     (str "<a" attrs# ">" ~name "</a>")))

(defmacro img
  [src & attrs]
  `(let [attrs# (param-attrs ~attrs)]
     (str "<img src=\"" ~src "\""attrs# "/>")))

