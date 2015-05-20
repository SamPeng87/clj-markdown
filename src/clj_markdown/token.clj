(ns clj-markdown.token
  (:use clj-markdown.tags)
  (:use clj-markdown.funcs))
;这个文件会被core引用
;一切以token开头的，会按meta中的优先级(:priority)排序后进行分析token的工作

(def regexBase {
                :newLines "^\\n+"
                :code "^( {4}[^\\n]+\\n*)+"
                :heading "^ *(#{1,6}) *([^\\n]+?) *#* *(?:\\n+|$)"
                :paragraph "^((?:[^\\n]+\\n?(?!hr|heading|lheading|blockquote|tag|def))+)\\n*"
                :hr "^( *[-*_]){3,} *(?:\\n+|$)"
                :lheading  "^([^\\n]+)\\n *(=|-){2,} *(?:\\n+|$)"
                :blockquote "^( *>[^\\n]+(\\n(?!def)[^\\n]+)*\\n*)+"
                :list "^( *)(bull) [\\s\\S]+?(?:hr|def|\\n{2,}(?! )(?!\\1bull )\\n*|\\s*$)"
                :html "^ *(?:comment *(?:\\n|\\s*$)|closed *(?:\\n{2,}|\\s*$)|closing *(?:\\n{2,}|\\s*$))"
                :def "^ *\\[([^\\]]+)\\]: *<?([^\\s>]+)>?(?: +[\\\"(]([^\\n]+)[\\\")])? *(?:\\n+|$)"
                :text "^[^\\n]+"
                :table "^ *\\|(.+)\\n *\\|( *[-:]+[-| :]*)\\n((?: *\\|.*(?:\\n|$))*)\\n*"
                :fences "^ *(`{3,}|~{3,}) *(\\S+)? *\\n([\\s\\S]+?)\\s*\\1 *(?:\\n+|$)"
                :nptable "^ *(\\S.*\\|.*)\\n *([-:]+ *\\|[-| :]*)\\n((?:.*\\|.*(?:\\n|$))*)\\n*"
                })

(def regexBullet "(?:[*+-]|\\d+\\.)")

(def regexListItems "^( *)(bull) [^\\n]*(?:\\n(?!\\1bull )[^\\n]*)*")

(def regexListItem (clojure.string/replace regexListItems "bull" regexBullet))

(def regexTags "(?!(?:a|em|strong|small|s|cite|q|dfn|abbr|data|time|code|var|samp|kbd|sub|sup|i|b|u|mark|ruby|rt|rp|bdi|bdo|span|br|wbr|ins|del|img)\\b)\\w+(?!:/|[^\\w\\s@]*@)\\b")

(defn executeTranslateProcess
  [src isTop isBlockQuote])
(declare token)



(def regexReplaced (-> regexBase
                       (replaceHashMap :blockquote (replaceRegex (regexBase :blockquote)
                                                                 ["def" (regexBase :def)]))
                       (replaceHashMap :list (replaceRegex (regexBase :list)
                                                           ["bull" regexBullet]
                                                           ["hr" "\\n+(?=\\1?(?:[-*_] *){3,}(?:\\n+|$))"]
                                                           ["def" (clojure.string/join "" ["\\n+(?="  (regexBase :def)  ")"])]))
                       (replaceHashMap :html (replaceRegex (regexBase :html)
                                                           ["comment" "<!--[\\s\\S]*?-->"]
                                                           ["closed" "<(tag)[\\s\\S]+?<\\/\\1>"]
                                                           ["closing" "<tag(?:\\\"[^\\\"]*\\\"|'[^']*'|[^'\\\">])*?>"]
                                                           ["tag" regexTags]))
                       (replaceHashMap :paragraph (replaceRegex (regexBase :paragraph)
                                                                ["hr" (regexBase :hr)]
                                                                ["heading" (regexBase :heading)]
                                                                ["lheading" (regexBase :lheading)]
                                                                ["blockquote" (regexBase :blockquote)]
                                                                ["tag" (clojure.string/join "" ["<" regexTags])]
                                                                ["def" (regexBase :def)]))
                       ))

(defmacro regex
  [name]
  (let [regexSource (regexReplaced name)]
    (re-pattern regexSource)))

(defn tokenNewlines
  ;换行符
  {:pattern  (regex :newLines)
   :isTop -1
   :isBlockquote -1
   }
  [cap & status]
  (println "token newline")
  (->NewLine))

(defn tokenCode
  {:pattern  (regex :code)
   :isTop -1
   :isBlockquote -1
   }
  [cap & status]
  (println "token code")
  (let [pattern (. java.util.regex.Pattern compile "^ {4}" java.util.regex.Pattern/MULTILINE) ]
    (->Code
      (clojure.string/replace
        (clojure.string/replace (first cap) pattern "")
        #"\n+$" "")
      nil
      )))

(defn tokenFences
  {
   :pattern (regex :fences)
   :isTop -1
   :isBlockquote -1
   }
  [cap & status]
  (println "token fences gfm code syntex")
  (->Code
    (get cap 3)
    (get cap 2)
    ))

(defn tokenHeading
  ;标题栏
  {
   :pattern  (regex :heading)
   :isTop -1
   :isBlockquote -1
   }
  [cap & status]
  (println "token heading")
  (->Heading
    (last cap)
    (count (get cap 1))
    ))

(defn tokenParagraph
  {
   :pattern  (regex :paragraph)
   :isTop 1
   :isBlockquote -1
   }
  [cap & status]
  (println "token paragraph")
  (let [body (get cap 1)
        endChar (get body (- (count body) 1))
        resultBody (if (= endChar \newline)
                     (subs body 0 (- (count body) 1))
                     body)]
    (->Paragraph resultBody)))

(defn tokenHr
  {
   :pattern  (regex :hr)
   :isTop -1
   :isBlockquote -1
   }
  [cap & status]
  (println "token hr")
  (->Hr))

(defn tokenlHeading
  {
   :pattern (regex :lheading)
   :isTop -1
   :isBlockquote -1
   }
  [cap & status]
  (println "token lHeading")
  (->Heading
    (get cap 1)
    {:depth (= (get cap 2) "=")}
    ))

(defn tokenBlockquote
  {
   :pattern  (regex :blockquote)
   :isTop -1
   :isBlockquote -1
   }
  [cap & status]
  (println "token blockquote")
  (let [
        statusTemp (apply hash-map status)
        pattern (. java.util.regex.Pattern compile "^ *> ?" java.util.regex.Pattern/MULTILINE)
        capTemp (clojure.string/replace (first cap) pattern "")
        tempResult (token capTemp (statusTemp :isTop) true)
        blockquoteResult (->Blockquote tempResult)]
     blockquoteResult))


(defn- regexMatchAll
  [string pattern]
  (let [matcher (re-matcher (. java.util.regex.Pattern
                               compile pattern java.util.regex.Pattern/MULTILINE)
                            string)]
    (loop [find (re-find matcher) ,result []]
      (if (nil? find)
        result
        (recur (re-find matcher) (conj result (first find)))))))


(defn- parseItemVector
  [cap bull isQuoteBlock]
  (let [l (count cap)]
    (loop [i 0, result [], next false]
      (if (>= i l)
        result
        (let [item (clojure.string/replace (get cap i) #" *([*+-]|\d+\.) +" "")
              endSpace (= (.indexOf item "\n ") -1)
              item (if endSpace
                     (.replaceAll item "^ {1,4}" "")
                     item)
              [next losse] ((fn
                              [next item i l]
                              (let [losse (or next (not (nil? (re-find #"\n\n(?!\s*$)" item))))]
                                (if (not= i (- l 1))
                                  (let [nextAnonymous (= (.charAt item (- (count item) 1)) (char \newline))
                                        losseAnonymous (if (not losse)
                                                         nextAnonymous
                                                         losse)]
                                    [nextAnonymous losseAnonymous])
                                  [next losse]
                                  )))
                             next item i l)
              ]
          (recur (inc i) (-> result
                             (conj (if losse
                                     "looseItemStart"
                                     "itemStart"
                                     ))
                             (into (token item false isQuoteBlock))
                             (conj "itemEnd")) next)
          )))))

(defn- tokenInternalTable
  [cap hasLeadingPipe]
  (let [
        headers (apply vector (.split (.replaceAll (get cap 1) "^ *| *\\| *$" "" ) " *\\| *"))
        aligns  (apply vector (.split (.replaceAll (get cap 2) "^ *| *\\| *$" "" ) " *\\| *"))
        cellses  (apply vector (.split (.replace (get cap 3) "(?: *\\| *)?\\n$" "") "\\n"))]
    (->Table
      headers
      ((fn
         [alignSource]
         (loop [nextAlign alignSource, result []]
           (if (nil? nextAlign)
             result
             (let [currentAlign (first nextAlign)
                   align (cond
                           (.matches currentAlign "^ *-+: *$") "right"
                           (.matches currentAlign "^ *:-+: *$") "center"
                           (.matches currentAlign "^ *:-+ *$") "left"
                           true nil)]
               (recur (next nextAlign) (conj result align)))))) aligns)
      ((fn
         [cellsSource]
         (loop [nextCells cellsSource, result []]
           (if (nil? nextCells)
             result
             (let [currentCells (apply vector (.split (if hasLeadingPipe
                                                        (.replaceAll (first nextCells) "^ *\\| *| *\\| *$" "")
                                                        (first nextCells)) " *\\| *"))]
               (recur (next nextCells) (conj result currentCells)))))) cellses))))

(defn tokenTable
  {:pattern  (regex :table)
   :isTop 1
   :isBlockquote -1
   }
  [cap & status]
  (tokenInternalTable cap true))

(defn tokenNpTable
  {
   :pattern  (regex :nptable)
   :isTop 1
   :isBlockquote -1
   }
  [cap & status]
  (tokenInternalTable cap false))

(defn tokenList
  {:pattern  (regex :list)
   :isTop -1
   :isBlockquote -1
   }
  [cap & status]
  (println "token list")
  (let [bull (get cap 2)
        status (apply hash-map status)
        ordered (> (count bull) 1)
        cap (regexMatchAll (first cap) regexListItem)
        itemVector (parseItemVector cap bull (status :isBlockquote))]
        (->Lists ordered itemVector)))

(defn tokenHtml
  {
   :pattern  (regex :html)
   :isTop -1
   :isBlockquote -1
   }
  [cap & status]
  (println "token html")
  (let [body (first cap)
        tagName (get cap 1)]
    (->Html
      body
      {:pre (or (= tagName "pre")
                (= tagName "script")
                (= tagName "style"))})))

(defn tokenDef
  {:pattern  (regex :def)
   :isTop 1
   :isBlockquote 0
   }
  [cap & status]
  (println "token def")
  (let [result (->Link (get cap 2)
                       (get cap 3)
                       (clojure.string/lower-case (get cap 1)))]
    result))

(defn tokenText
  {
   :pattern  (regex :text)
   :isTop -1
   :isBlockquote -1
   }
  [cap & status]
  (println "token text")
  (->Text (first cap)))

(defn- resultJoin
  ([]
    [])
  ([source target]
    (if (or (list? target) (vector? target))
      (into source target)
      (conj source target))))

(defn token
  [src isTop isBlockquote]
  (let [tokenMethod [#'tokenNewlines
                     #'tokenCode
                     #'tokenFences
                     #'tokenHeading
                     #'tokenNpTable
                     #'tokenlHeading
                     #'tokenHr
                     #'tokenBlockquote
                     #'tokenList
                     #'tokenHtml
                     #'tokenDef
                     #'tokenTable
                     #'tokenParagraph
                     #'tokenText]]
    (whileTranslateString
      src
      {:isTop isTop
       :isBlockquote isBlockquote}
      tokenMethod
      executeInputByCheck
      checkMethodPattern
      resultJoin)))

