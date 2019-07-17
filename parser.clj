(ns grammar.parser)

;Global configuation variable
(def global-config (atom {:state ""
                          :string ""
                          :registers (atom {})
                          :hold-register '()
                          :level ""
                          :pop-config ""
                          :push-acts ""
                          :sendr-acts (atom {})
                          :liftr-acts (atom {} )}))

;Default starting state in grammar
(def global-state (atom ""))

;Default trace level
(def trace-level (atom 0))

(def parse-trees (atom nil))

;Hashtable for the ATN arcs
;(def atn-arcs-ht (atom {:s/_ '()
;                        :s/np '()
;                        :s/aux '()
;                        :np/_ '()
;                        :np/adj '()
;                        :np/art '()
;                        :np/quant '()
;                        :np/n '()
;                        :pp/_ '()
;                        :pp/prep '()
;                        :pp/np '()
;                        :vp/v '()
;                        :vp/head '()
;                        :vp/obj '()
;                        :vp/vp '()
;                        :vp/by '()
;                        :vp/to '()
;                        :comp/_ '()}))

(def atn-arcs-ht (atom nil))

(defrecord config
  [state                                                    ;state of the ATN
   string                                                   ;current state of the input string
   registers                                                ;current register values (assoc list)
   hold-register                                            ;list of hold pairs (category form) pairs
   level                                                    ;current level of grammer
   pop-config                                               ;back link to config PUSHed, CALLed, or RCALLed
   push-acts                                                ;post acts used when pop-ing from a PUSH, CALL, or RCALL
   sendr-acts                                               ;assoc list of (register value) to be set on entry to this config
   liftr-acts])                                             ;assoc list of (register value) to be set on exit from this config

(defmacro cat-category [arc]
  `(second (~arc)))
(defmacro cat-test [arc]
  `(third (~arc)))
(defmacro cat-actions [arc]
  `(rest (rest (rest (~arc)))))

(defmacro wrd-word [arc]
  `(first (rest (~arc))))
(defmacro wrd-test [arc]
  `(first (rest (rest (~arc)))))
(defmacro wrd-actions [arc]
  `(rest (rest (rest (~arc)))))

(defmacro mem-list [arc]
  `(first (rest (~arc))))
(defmacro mem-test [arc]
  `(first (rest (rest (~arc)))))
(defmacro mem-actions [arc]
  `(rest (rest (rest (~arc)))))

(defmacro push-state [arc]
  `(first (rest (~arc))))
(defmacro push-test [arc]
  `(first (rest (rest (~arc)))))
(defmacro push-actions [arc]
  `(rest (rest (rest (~arc)))))

(defmacro vir-constit-type [arc]
  `(first (rest (~arc))))
(defmacro vir-test [arc]
  `(first (rest (rest (~arc)))))
(defmacro vir-actions [arc]
  `(rest (rest (rest (~arc)))))

(defmacro jump-next-state [arc]
  `(first (rest (~arc))))
(defmacro jump-test [arc]
  `(first (rest (rest (~arc)))))
(defmacro jump-actions [arc]
  `(rest (rest (rest (~arc)))))

(defmacro pop-form [arc]
  `(first (rest (~arc))))
(defmacro pop-test [arc]
  `(first (rest (rest (~arc)))))
(defmacro pop-actions [arc]
  `(rest (rest (rest (~arc)))))

(defmacro tst-label [arc]
  `(first (rest (~arc))))
(defmacro tst-test [arc]
  `(first (rest (rest (~arc)))))
(defmacro tst-actions [arc]
  `(rest (rest (rest (~arc)))))

(defmacro to-state [arc]
  `(first (first (rest (~arc)))))
(defmacro to-form [arc]
  `(first (rest (first (rest (~arc))))))
(defmacro to-test [arc]
  `(first (rest (rest (~arc)))))
(defmacro to-actions [arc]
  `(rest (rest (rest (~arc)))))

(defmacro call-state [arc]
  `(second (~arc)))
(defmacro call-form [arc]
  `(first (rest (rest (~arc)))))
(defmacro call-test [arc]
  `(first (rest (rest (rest (~arc))))))
(defmacro call-actions [arc]
  `(rest (rest (rest (rest (~arc))))))

(defmacro rcall-state [arc]
  `(second (~arc)))
(defmacro rcall-form [arc]
  `(first (rest (rest (~arc)))))
(defmacro rcall-test [arc]
  `(first (rest (rest (rest (~arc))))))
(defmacro rcall-actions [arc]
  `(rest (rest (rest (rest (~arc))))))

(defmacro set-state
  ([value] `(swap! global-config assoc :state ~value))
  ([value config] `(swap! ~config assoc :state ~value)) )

(defmacro get-state
  ([] `(:state global-config))
  ([config] `(:state ~config)))

(defmacro set-string
  ([value] `(swap! global-config assoc :string ~value))
  ([value config] `(swap! ~config assoc :string ~value)) )

(defmacro get-string
  ([] `(:string global-config))
  ([config] `(:string ~config)))

(defmacro set-registers
  ([value] `(swap! global-config assoc :registers ~value))
  ([value config] `(swap! ~config assoc :registers ~value)) )

(defmacro get-registers
  ([] `(:registers global-config))
  ([config] `(:registers ~config)))

(defmacro set-hold-register
  ([value] `(swap! global-config assoc :hold-register ~value))
  ([value config] `(swap! ~config assoc :hold-register ~value)) )

(defmacro get-hold-register
  ([] `(:hold-register global-config))
  ([config] `(:hold-register ~config)))

(defmacro set-level
  ([value] `(swap! global-config assoc :level ~value))
  ([value config] `(swap! ~config assoc :level ~value)) )

(defmacro get-level
  ([] `(:level global-config))
  ([config] `(:level ~config)))

(defmacro set-pop-config
  ([value] `(swap! global-config assoc :pop-config ~value))
  ([value config] `(swap! ~config assoc :pop-config ~value)) )

(defmacro get-pop-config
  ([] `(:pop-config global-config))
  ([config] `(:pop-config ~config)))

(defmacro set-push-acts
  ([value] `(swap! global-config assoc :push-acts ~value))
  ([value config] `(swap! ~config assoc :push-acts ~value)) )

(defmacro get-push-acts
  ([] `(:push-acts global-config))
  ([config] `(:pop-config ~config)))

(defmacro set-sendr-acts
  ([value] `(swap! global-config assoc :sendr-acts ~value))
  ([value config] `(swap! ~config assoc :sendr-acts ~value)) )

(defmacro get-sendr-acts
  ([] `(:sendr-acts global-config))
  ([config] `(:sendr-acts ~config)))

(defmacro set-liftr-actions
  ([value] `(swap! global-config assoc :liftr-acts ~value))
  ([value config] `(swap! ~config assoc :liftr-acts ~value)) )

(defmacro get-liftr-actions
  ([] `(:liftr-acts global-config))
  ([config] `(:liftr-acts ~config)))

(defmacro putarc [state arc]
  `(swap! atn-arcs-ht assoc (keyword ~state) (cons ~arc ((keyword ~state) atn-arcs-ht))))

(defmacro getarcs [state]
  `((keyword ~state) atn-arcs-ht))

(defmacro setr [register & forms]
  `(set-registers
     ()))

(defmacro setrq [register & form]
  )

(defmacro addr [register & forms]
  `(let [contents (get-in global-config [:registers (keyword ~register)])
         forms (evaluate-forms ~forms)
         newcontents (conj forms contents)]
     (swap! global-config update-in [:registers] assoc (keyword ~register) newcontents)
     )
  )

(defmacro addl [register & forms]
  `(let [contents (get-in global-config [:registers (keyword ~register)])
         forms (evaluate-forms ~forms)
         newcontents (conj forms contents)]
     (swap! global-config update-in [:registers] assoc (keyword ~register) newcontents)
     )
  )

(defmacro sendr [register & forms]
  `())

(defmacro sendrq [register & forms]
  `())

(defmacro liftr [register & forms]
  `('(~state)))

(defmacro hold [constit-type form]
  `(set-hold-register (cons (list ~category
                                  (list (setup-forms (evaluate-form ~form)))
                                  (get-level))
                            (get-hold-register))))

(defn any-holds? []
  (binding global-config)
  (let [holdr (get-hold-register)
        level (get-level)]
    (if (nil? holdr)
      true
      ((fn [x] (equal (first (rest (rest x))) level)) holdr))))

(defmacro to
  ([state]
   `())
  ([state form]
   `()))

(defmacro jump [state]
  `())

;;(defmacro verify [form] `())

(defmacro getr
  ([register]
   `())
  ([register level])
  `())

(defmacro getf
  ([feature]
   `())
  ([feature word]
   `()))

(defmacro buildq [exp]
  `())

(defn bldq [exp]
  `())

(defn setup-forms [forms]
  (if (atom forms)
    forms
    (if (rest forms)
      (if parse-trees
        forms (flatten forms))
      (if parse-trees
        (first forms)
        (flatten (first forms))))))

;;Evaluates an ATN form
(defn evaluate-forms [s-exp]
  )

;;Clojure has a function that yields the unevaluated form (quote(____))
;;(defmacro quote [value])

;;tests on the arcs
(defmacro nullr [register]
  `(= (get-in global-config [:registers (keyword ~register)]) nil))

(defmacro end-of-sentence []
  (binding [global-config]))

;;DO LATER/WHEN IT COMES UP
;;(defmacro checkf [feature value])
;;(defmacro catcheck [word cat])
;;(defmacro x-agree [form form])
;;(defmacro x-start [])














