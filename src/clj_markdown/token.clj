(ns clj-markdown.token
  {:use clj-markdown.core})
;这个文件会被core引用
;一切以token开头的，会按meta中的优先级(:priority)排序后进行分析token的工作

(def tag (create-struct :name :body :attr :children :class :style))
(def link (create-struct :name :href :title))
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

(def regexListItem "^( *)(bull) [^\\n]*(?:\\n(?!\\1bull )[^\\n]*)*")

(def regexListItem (clojure.string/replace regexListItem "bull" regexBullet))

(def regexTags "(?!(?:a|em|strong|small|s|cite|q|dfn|abbr|data|time|code|var|samp|kbd|sub|sup|i|b|u|mark|ruby|rt|rp|bdi|bdo|span|br|wbr|ins|del|img)\\b)\\w+(?!:/|[^\\w\\s@]*@)\\b")

(defn executeTranslateProcess
  [src isTop isBlockQuote])



(defn- replaceRegex
  [regexSource & replaceTarget]
  (let [replaceData (apply list replaceTarget)
        replaceResult (reduce (fn
                                [sourceString [name value]]
                                (clojure.string/replace sourceString name value))
                              regexSource replaceData)]
    replaceResult))

(defn- replaceHashMap
  [source key target]
  (assoc source key target))


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
   :priority 1
   :continue true}
  [src result isTop isQuoteBlock cap]
  (println "token newline")
  [(subs src (count (first cap)))
   (conj result (struct tag "newline"))
   isTop isQuoteBlock])

(defn tokenCode
  {:pattern  (regex :code)
   :priority 2
   :continue false}
  [src result isTop isQuoteBlock cap]
  (println "token code")
  (let [pattern (. java.util.regex.Pattern compile "^ {4}" java.util.regex.Pattern/MULTILINE) ]
    [(subs src (count (first cap)))
     (conj result (struct tag "code"
                          (clojure.string/replace
                            (clojure.string/replace (first cap) pattern "")
                            #"\n+$" "")
                          ))
     isTop isQuoteBlock]))

(defn tokenFences
  {:pattern (regex :fences)
   :continue false}
  [src result isTop isQuoteBlock cap]
  (println "token fences gfm code syntex")
  [(subs src (count (first cap)))
   (conj result (struct tag "code"
                        (get cap 3)
                        {:lang (get cap 2)}
                        ))
   isTop isQuoteBlock])

(defn tokenHeading
  ;标题栏
  {:pattern  (regex :heading)
   :priority 3
   :continue false
   }
  [src result isTop isQuoteBlock cap]
  (println "token heading")
  [(subs src (count (first cap)))
   (conj result (struct tag "heading"
                        (last cap)
                        {:depth (count (get cap 1))}
                        ))
   isTop isQuoteBlock])

(defn tokenParagraph
  {:pattern  (regex :paragraph)
   :priority 3
   :continue false}
  [src result isTop isQuoteBlock cap]
  (println "token paragraph")
  (let [body (get cap 1)
        endChar (get body (- (count body) 1))
        resultBody (if (= endChar \newline)
                     (subs body 0 (- (count body) 1))
                     body)]
    [(subs src (count (first cap)))
     (conj result (struct tag "paragraph" resultBody))
     isTop isQuoteBlock]))

(defn tokenHr
  {:pattern  (regex :hr)
   :priority 4
   :continue false}
  [src result isTop isQuoteBlock cap]
  (println "token hr")
  [(subs src (count (first cap)))
   (conj result (struct tag "hr"))
   isTop isQuoteBlock])

(defn tokenlHeading
  {:pattern (regex :lheading)
   :priority 6
   :continue false}
  [src result isTop isQuoteBlock cap]
  (println "token lHeading")
  [(subs src (count (first cap)))
   (conj result (struct tag "heading"
                        (get cap 1)
                        {:depth (= (get cap 2) "=")}
                        ))
   isTop isQuoteBlock])

(defn tokenBlockquote
  {:pattern  (regex :blockquote)
   :priority 7
   :continue false}
  [src result isTop isQuoteBlock cap]
  (println "token blockquote")
  (println "blockquote start")
  (let [src (subs src (count (first cap)))
        pattern (. java.util.regex.Pattern compile "^ *> ?" java.util.regex.Pattern/MULTILINE)
        capTemp (clojure.string/replace (first cap) pattern "")
        tempResult (executeTranslateProcess capTemp isTop true)
        blockquoteResult (-> []
                             (conj (struct tag "blockquote_start"))
                             (into tempResult)
                             (conj (struct tag "blockquote_end")))]

    (println "blockquote end")
    [src
     (into result blockquoteResult)
     isTop isQuoteBlock]))


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
                             (conj (struct tag (if losse
                                                 "loose_item_start"
                                                 "list_item_start")))
                             (into (executeTranslateProcess item false isQuoteBlock))
                             (conj (struct tag "list_item_end"))) next)
          )))))

(defn- tokenInternalTable
  [src result isTop isQuoteBlock cap hasLeadingPipe]
  (if isTop
    (let [src (subs src (count (first cap)))
          headers (apply vector (.split (.replaceAll (get cap 1) "^ *| *\\| *$" "" ) " *\\| *"))
          aligns  (apply vector (.split (.replaceAll (get cap 2) "^ *| *\\| *$" "" ) " *\\| *"))
          cellses  (apply vector (.split (.replace (get cap 3) "(?: *\\| *)?\\n$" "") "\\n"))]
      [src
       (conj result (struct tag "table"
                            {:header headers
                             :aligns ((fn
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
                             :cells ((fn
                                       [cellsSource]
                                       (loop [nextCells cellsSource, result []]
                                         (if (nil? nextCells)
                                           result
                                           (let [currentCells (apply vector (.split (if hasLeadingPipe
                                                                 (.replaceAll (first nextCells) "^ *\\| *| *\\| *$" "")
                                                                 (first nextCells)) " *\\| *"))]
                                             (recur (next nextCells) (conj result currentCells)))))) cellses)}))
       isTop isQuoteBlock])))

(defn tokenTable
  {:pattern  (regex :table)
   :priority 8
   :continue false}
  [src result isTop isQuoteBlock cap]
  (tokenInternalTable src result isTop isQuoteBlock cap true))

(defn tokenNpTable
  {:pattern  (regex :nptable)
   :priority 8
   :continue false}
  [src result isTop isQuoteBlock cap]
  (tokenInternalTable src result isTop isQuoteBlock cap false))

(defn tokenList
  {:pattern  (regex :list)
   :priority 8
   :continue false}
  [src result isTop isQuoteBlock cap]
  (println "token list")
  (let [src (subs src (count (first cap)))
        bull (get cap 2)
        listVector (conj []
                         (struct tag "list_start" {:ordered (> (count bull) 1)}))
        cap (regexMatchAll (first cap) regexListItem)
        itemVector (parseItemVector cap bull isQuoteBlock)]
    [src (into result (-> listVector
                          (conj itemVector)
                          (conj (struct tag "list_end"))))
     isTop isQuoteBlock]))

(defn tokenHtml
  {:pattern  (regex :html)
   :priority 9
   :continue false}
  [src result isTop isQuoteBlock cap]
  (println "token html")
  (let [body (first cap)
        match (count body)
        tagName (get cap 1)]
    [(subs src match)
     (conj result (struct tag "html"
                          body
                          {:pre (or (= tagName "pre")
                                    (= tagName "script")
                                    (= tagName "style"))}))
     isTop isQuoteBlock]))

(defn tokenDef
  {:pattern  (regex :def)
   :priority 10
   :continue false}
  [src result isTop isQuoteBlock cap]
  (println "token def")
  (if (and (not isQuoteBlock) isTop)
    (let [result (conj result (struct link (clojure.string/lower-case (get cap 1))
                                      (get cap 2)
                                      (get cap 3)))]
      [(subs src (count (first cap)))
       result
       isTop isQuoteBlock])))

(defn tokenText
  {:pattern  (regex :text)
   :priority 11
   :continue false}
  [src result isTop isQuoteBlock cap]
  (println "token text")
  [(subs src (count (first cap)))
   (conj result (struct tag "text"
                        (first cap)))
   isTop isQuoteBlock])

(def tokenProcess [
                   #'tokenNewlines
                   #'tokenCode
                   #'tokenFences
                   #'tokenHeading
                   #'tokenNpTable
                   #'tokenHr
                   #'tokenBlockquote
                   #'tokenList
                   #'tokenHtml
                   #'tokenDef
                   #'tokenTable
                   #'tokenParagraph
                   #'tokenText])

(defn chairMethod
  "从->宏里面修改来的。因为我需要一个后面的参数也是动态的数组，不是写死的"
  {:added "1.0"}
  [x  forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                       (form x))]
        (if ((meta threaded) :continue)
          (recur threaded (next forms))
          threaded)
        )
      x)))


(defn partialFuncCheck
  "创建处理方法的结合"
  [executeFuncNamespace]
  (let [result (partial (defn tmpFunc
                          [funcName [src result isTop isQuoteBlock]]
                          (let [metaData (meta funcName)
                                pattern (metaData :pattern)
                                matcher (re-matcher pattern src)
                                cap (re-find matcher)]
                            (if (nil? cap)
                              (let [rResult [src result isTop isQuoteBlock]]
                                (with-meta rResult {:continue true}))
                              (let [rResult (funcName src result isTop isQuoteBlock cap)]
                                (with-meta rResult {:continue (metaData :continue)}))))
                          ) executeFuncNamespace)]
    result))

(defn createPartialFuncs
  []
  (let [funcs tokenProcess]
    (map partialFuncCheck funcs)))

(defn executeTranslateProcess
  [src isTop isQuoteBlock]
  (loop [src src, result [], isTop isTop, isQuoteBlock isQuoteBlock]
    (if (= src "")
      result
      (let [translateFunctions (createPartialFuncs)
            [rSrc rResult isTop isQuoteBlock] (chairMethod [src result isTop isQuoteBlock]
                                                           translateFunctions)]
        (recur rSrc rResult isTop isQuoteBlock)))))
