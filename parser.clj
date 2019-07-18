(ns grammar.parser)

;Global configuation variable
(def ^{:dynamic true} *config* nil)
;(def global-config (atom {:state ""
;                          :string ""
;                          :registers (atom {})
;                          :hold-register '()
;                          :level ""
;                          :pop-config ""
;                          :push-acts ""
;                          :sendr-acts (atom {})
;                          :liftr-acts (atom {} )}))

;Global parse trees
;(def parse-trees (atom nil))
(def ^{:dynamic true} *parse-trees* nil)

;Default starting state in grammar
(def global-state (atom ""))
;(def ^{:dynamic true} *global-state* nil)

;Default trace level
(def trace-level (atom 0))
;(def ^{:dynamic true} *trace-level* nil)

;;Hashtable for the ATN arcs
;;How it should look after initializing
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

;(def atn-arcs-ht (atom nil))
(def ^{:dynamic true} *atn-arcs-ht* nil)

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

(defn make-configuration
  [& {:keys [state string registers hold-register level pop-config push-acts sendr-acts liftr-acts]
      :or {state "", string "", registers (atom {}), hold-register (), level "",
           pop-config "", push-acts "", sendr-acts (atom {}), liftr-acts (atom {})}}]
  (config. state string registers hold-register level pop-config push-acts sendr-acts liftr-acts))

(defmacro cat-category [arc]
  `(second (~arc)))
(defmacro cat-test [arc]
  `(nth (~arc) 2))
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
  "Set the registers to a given value, either in the provided configuration
   or, if no configuration was provided, the global configuration."
  [value & {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `(reset! (:state ~configuration) ~value)
    `(reset! (:state *config*) ~value))) ;; Leave @*config* unevaluated!

;; Note: Assumes *config* has a non-nil value.
(defmacro get-state
  "Get the registers from a provided configuration, or, if none is provided, then
   the global configuration."
  [& {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `@(:state ~configuration)
    `@(:state *config*))) ;; Leave *config* unevaluated!

(defmacro set-string
  "Set the registers to a given value, either in the provided configuration
   or, if no configuration was provided, the global configuration."
  [value & {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `(reset! (:string ~configuration) ~value)
    `(reset! (:string *config*) ~value))) ;; Leave @*config* unevaluated!

;; Note: Assumes *config* has a non-nil value.
(defmacro get-string
  "Get the registers from a provided configuration, or, if none is provided, then
   the global configuration."
  [& {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `@(:string ~configuration)
    `@(:string *config*))) ;; Leave *config* unevaluated!

(defmacro set-registers
  "Set the registers to a given value, either in the provided configuration
   or, if no configuration was provided, the global configuration."
  [value & {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `(reset! (:registers ~configuration) ~value)
    `(reset! (:registers *config*) ~value)))

;; Note: Assumes *config* has a non-nil value.
(defmacro get-registers
  "Get the registers from a provided configuration, or, if none is provided, then
   the global configuration."
  [& {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `@(:registers ~configuration)
    `@(:registers *config*))) ;; Leave *config* unevaluated!

(defmacro set-hold-register
  "Set the registers to a given value, either in the provided configuration
   or, if no configuration was provided, the global configuration."
  [value & {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `(reset! (:hold-register ~configuration) ~value)
    `(reset! (:hold-register *config*) ~value))) ;; Leave @*config* unevaluated!

;; Note: Assumes *config* has a non-nil value.
(defmacro get-hold-register
  "Get the registers from a provided configuration, or, if none is provided, then
   the global configuration."
  [& {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `@(:hold-register ~configuration)
    `@(:hold-register *config*))) ;; Leave *config* unevaluated!

(defmacro set-level
  "Set the registers to a given value, either in the provided configuration
   or, if no configuration was provided, the global configuration."
  [value & {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `(reset! (:level ~configuration) ~value)
    `(reset! (:level *config*) ~value))) ;; Leave @*config* unevaluated!

;; Note: Assumes *config* has a non-nil value.
(defmacro get-level
  "Get the registers from a provided configuration, or, if none is provided, then
   the global configuration."
  [& {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `@(:level ~configuration)
    `@(:level *config*))) ;; Leave *config* unevaluated!

(defmacro set-pop-config
  "Set the registers to a given value, either in the provided configuration
   or, if no configuration was provided, the global configuration."
  [value & {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `(reset! (:pop-config ~configuration) ~value)
    `(reset! (:pop-config *config*) ~value))) ;; Leave @*config* unevaluated!

;; Note: Assumes *config* has a non-nil value.
(defmacro get-pop-config
  "Get the registers from a provided configuration, or, if none is provided, then
   the global configuration."
  [& {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `@(:pop-config ~configuration)
    `@(:pop-config *config*))) ;; Leave *config* unevaluated!

(defmacro set-push-acts
  "Set the registers to a given value, either in the provided configuration
   or, if no configuration was provided, the global configuration."
  [value & {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `(reset! (:push-acts ~configuration) ~value)
    `(reset! (:push-acts *config*) ~value))) ;; Leave @*config* unevaluated!

;; Note: Assumes *config* has a non-nil value.
(defmacro get-push-acts
  "Get the registers from a provided configuration, or, if none is provided, then
   the global configuration."
  [& {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `@(:push-acts ~configuration)
    `@(:push-acts *config*))) ;; Leave *config* unevaluated!

(defmacro set-sendr-acts
  "Set the registers to a given value, either in the provided configuration
   or, if no configuration was provided, the global configuration."
  [value & {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `(reset! (:sendr-acts ~configuration) ~value)
    `(reset! (:sendr-acts *config*) ~value))) ;; Leave @*config* unevaluated!

;; Note: Assumes *config* has a non-nil value.
(defmacro get-sendr-acts
  "Get the registers from a provided configuration, or, if none is provided, then
   the global configuration."
  [& {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `@(:sendr-acts ~configuration)
    `@(:sendr-acts *config*))) ;; Leave *config* unevaluated!

(defmacro set-liftr-acts
  "Set the registers to a given value, either in the provided configuration
   or, if no configuration was provided, the global configuration."
  [value & {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `(reset! (:liftr-acts ~configuration) ~value)
    `(reset! (:liftr-acts *config*) ~value))) ;; Leave @*config* unevaluated!

;; Note: Assumes *config* has a non-nil value.
(defmacro get-liftr-acts
  "Get the registers from a provided configuration, or, if none is provided, then
   the global configuration."
  [& {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `@(:liftr-acts ~configuration)
    `@(:liftr-acts *config*))) ;; Leave *config* unevaluated!

(defmacro putarc [state arc]
  `(swap! atn-arcs-ht assoc (keyword ~state) (cons ~arc ((keyword ~state) atn-arcs-ht))))

(defmacro getarcs [state]
  `((keyword ~state) atn-arcs-ht))

(defn setup-forms
  ""
  [forms]
  (if (or (symbol? forms) (char? forms) (number? forms) (string? forms)) ;; Equivalent of atoms in Lisp.
    forms
    (if (rest forms)
      (if *parse-trees*
        forms
        (flatten forms))
      (if *parse-trees*
        (first forms)
        (flatten (first forms))))))

(defn internal-getr
  "Retrieve the specific register from the register map."
  [register]
  (get (get-registers) register))

;;Evaluates an ATN form
(defn evaluate-form
  "Given an S-expression, evaluates it by:
    If it's a string or number, just returning those values;
	If it names a register, returning the register value;
	If it is *, returning the register value of *;
	If it is a list, evaluating it as a Clojure form;
	If it is bound to a variable, returning its value;
	Otherwise, nil."
  [s-exp]
  (cond
    (number? s-exp) s-exp
    (string? s-exp) s-exp
    (internal-getr s-exp) (internal-getr s-exp)
    (= s-exp '*) (internal-getr '*)
    (seq? s-exp) (eval s-exp) ;; No need for this peval thing.
    (resolve s-exp) (resolve s-exp)
    :default nil))

(defmacro setr [register & forms]
  `(set-registers (assoc (get-registers)                    ;;map
                    '~register                              ;;key
                    (setup-forms (map evaluate-form '~forms))))) ;;val(s)

(defmacro setrq [register & forms]
  `(set-registers (assoc (get-registers)                    ;;map
                    '~register                              ;;key
                    (setup-forms (map '~forms)))))          ;;val(s)

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
  `(if ~forms
     `(set-sendr-acts (assoc (get-sendr-acts)               ;map
                       '~register                           ;key
                       (setup-forms (map evaluate-form '~forms)))) ;val(s)
     `(set-sendr-acts (assoc (get-sendr-acts)               ;map
                        '~register                          ;key
                        (internal-getr ~register)))))       ;val(s)

(defmacro sendrq [register & forms]
  `(set-sendr-acts (assoc (get-sendr-acts)                  ;map
                    '~register                              ;key
                    (setup-forms (map '~forms)))))          ;val(s)

(defmacro liftr [register & forms]
  `(if ~forms
     `(set-liftr-acts (assoc (get-liftr-acts)               ;map
                        '~register                          ;key
                        (setup-forms (map evaluate-form '~forms)))) ;val(s)
     `(set-liftr-acts (assoc (get-liftr-acts)               ;map
                        '~register                          ;key
                        (internal-getr ~register)))))       ;val(s)

(defmacro hold [constit-type form]
  `(set-hold-register (cons (list ~constit-type
                                  (list (setup-forms (evaluate-form ~form)))
                                  (get-level))
                            (get-hold-register))))

;(defn any-holds? []
;  (binding global-config)
;  (let [holdr (get-hold-register)
;        level (get-level)]
;    (if (nil? holdr)
;      true
;      ((fn [x] (equal (first (rest (rest x))) level)) holdr))))

;(defmacro to
;  ([state]
;   `())
;  ([state form]
;   `()))
;
;(defmacro jump [state]
;  `())
;
;;;(defmacro verify [form] `())
;
;(defmacro getr
;  ([register]
;   `())
;  ([register level])
;  `())
;
;(defmacro getf
;  ([feature]
;   `())
;  ([feature word]
;   `()))
;
;(defmacro buildq [exp]
;  `())
;
;(defn bldq [exp]
;  `())


;;Clojure has a function that yields the unevaluated form (quote(____))
;;(defmacro quote [value])

;;tests on the arcs
(defmacro nullr [register]
  `(= (get-in global-config [:registers (keyword ~register)]) nil))

;(defmacro end-of-sentence []
;  (binding [global-config]))

;;DO LATER/WHEN IT COMES UP
;;(defmacro checkf [feature value])
;;(defmacro catcheck [word cat])
;;(defmacro x-agree [form form])
;;(defmacro x-start [])














