(ns grammar.parser)

(require '[clojure.string :as str])

;Global configuation variable
(def ^{:dynamic true} *config* nil)

;Global parse trees
(def ^{:dynamic true} *parse-trees* nil)

(def ^{:dynamic true} *all-parses* nil)

(def ^{:dynamic true} *frags* nil)

;Default starting state in grammar
(def global-state (atom ""))

;Default trace level
(def trace-level (atom 0))

(def punctuation ["," ":" ";" "." "!" "?"
                  "(" ")" "[" "]" "{" "}"
                  "'" "`" "/" "#" "^" "|"])

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

(def ^{:dynamic true} *lexicon* nil)

(def *white-space*
  "The set of characters that constitute white space. White space terminates
  words and gets skipped and thrown away."
  '(#\space #\tab #\newline))

(def *quote-chars*
  "The set of quotation characters. Between quotation characters all special
  characters such as punctuation characters or white space loose their special
  meaning. Everything between quotation characters gets collected into a single
  token. A quotation can contain quotation characters that were preceded
  by an escape character."
  "\"")

(def *escape-chars*
  "The set of escape characters. An escape character removes any special
  meaning of the next character."
  "\\")

(defrecord config
  [state                                                    ;state of the ATN
   string                                                   ;current state of the input string
   registers                                                ;current register values (assoc list)
   hold-register                                            ;list of hold pairs (category form) pairs
   level                                                    ;current level of grammar
   pop-config                                               ;back link to config PUSHed, CALLed, or RCALLed
   push-acts                                                ;post acts used when pop-ing from a PUSH, CALL, or RCALL
   sendr-acts                                               ;assoc list of (register value) to be set on entry to this config
   liftr-acts])                                             ;assoc list of (register value) to be set on exit from this config

(defn make-configuration
  [& {:keys [state string registers hold-register level pop-config push-acts sendr-acts liftr-acts]
      :or {state "", string "", registers (atom {}), hold-register (atom {}), level "",
           pop-config "", push-acts (atom {}), sendr-acts (atom {}), liftr-acts (atom {})}}]
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
  "Set the state to a given value, either in the provided configuration
   or, if no configuration was provided, the global configuration."
  [value & {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `(reset! (:state ~configuration) ~value)
    `(reset! (:state *config*) ~value)))

;Note: Assumes *config* has a non-nil value.
(defmacro get-state
  "Get the state from a provided configuration, or, if none is provided, then
   the global configuration."
  [& {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `@(:state ~configuration)
    `@(:state *config*)))

(defmacro set-string
  "Set the string to a given value, either in the provided configuration
   or, if no configuration was provided, the global configuration."
  [value & {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `(reset! (:string ~configuration) ~value)
    `(reset! (:string *config*) ~value)))

;Note: Assumes *config* has a non-nil value.
(defmacro get-string
  "Get the string from a provided configuration, or, if none is provided, then
   the global configuration."
  [& {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `@(:string ~configuration)
    `@(:string *config*)))

(defmacro set-registers
  "Set the registers to a given value, either in the provided configuration
   or, if no configuration was provided, the global configuration."
  [value & {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `(reset! (:registers ~configuration) ~value)
    `(reset! (:registers *config*) ~value)))

;Note: Assumes *config* has a non-nil value.
(defmacro get-registers
  "Get the registers from a provided configuration, or, if none is provided, then
   the global configuration."
  [& {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `@(:registers ~configuration)
    `@(:registers *config*)))

(defmacro set-push-acts
  "Set the push-acts to a given value, either in the provided configuration
   or, if no configuration was provided, the global configuration."
  [value & {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `(reset! (:push-acts ~configuration) ~value)
    `(reset! (:push-acts *config*) ~value)))

;Note: Assumes *config* has a non-nil value.
(defmacro get-push-acts
  "Get the push-acts from a provided configuration, or, if none is provided, then
   the global configuration."
  [& {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `@(:push-acts ~configuration)
    `@(:push-acts *config*)))

(defmacro set-sendr-acts
  "Set the sendr-acts to a given value, either in the provided configuration
   or, if no configuration was provided, the global configuration."
  [value & {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `(reset! (:sendr-acts ~configuration) ~value)
    `(reset! (:sendr-acts *config*) ~value)))

;Note: Assumes *config* has a non-nil value.
(defmacro get-sendr-acts
  "Get the sendr-acts from a provided configuration, or, if none is provided, then
   the global configuration."
  [& {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `@(:sendr-acts ~configuration)
    `@(:sendr-acts *config*)))

(defmacro set-liftr-acts
  "Set the liftr-acts to a given value, either in the provided configuration
   or, if no configuration was provided, the global configuration."
  [value & {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `(reset! (:liftr-acts ~configuration) ~value)
    `(reset! (:liftr-acts *config*) ~value)))

;Note: Assumes *config* has a non-nil value.
(defmacro get-liftr-acts
  "Get the liftr-acts from a provided configuration, or, if none is provided, then
   the global configuration."
  [& {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `@(:liftr-acts ~configuration)
    `@(:liftr-acts *config*)))

(defmacro set-hold-register
  "Set the hold-register to a given value, either in the provided configuration
   or, if no configuration was provided, the global configuration."
  [value & {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `(reset! (:hold-register ~configuration) ~value)
    `(reset! (:hold-register *config*) ~value)))

;Note: Assumes *config* has a non-nil value.
(defmacro get-hold-register
  "Get the hold-register from a provided configuration, or, if none is provided, then
   the global configuration."
  [& {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `@(:hold-register ~configuration)
    `@(:hold-register *config*)))

(defmacro set-level
  "Set the level to a given value, either in the provided configuration
   or, if no configuration was provided, the global configuration."
  [value & {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `(reset! (:level ~configuration) ~value)
    `(reset! (:level *config*) ~value)))

;Note: Assumes *config* has a non-nil value.
(defmacro get-level
  "Get the level from a provided configuration, or, if none is provided, then
   the global configuration."
  [& {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `@(:level ~configuration)
    `@(:level *config*)))

(defmacro set-pop-config
  "Set the pop-config to a given value, either in the provided configuration
   or, if no configuration was provided, the global configuration."
  [value & {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `(reset! (:pop-config ~configuration) ~value)
    `(reset! (:pop-config *config*) ~value)))

;Note: Assumes *config* has a non-nil value.
(defmacro get-pop-config
  "Get the pop-config from a provided configuration, or, if none is provided, then
   the global configuration."
  [& {:keys [configuration] :or {configuration nil}}]
  (if configuration
    `@(:pop-config ~configuration)
    `@(:pop-config *config*)))

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
;Given an S-expression, evaluates it by:
; If it's a string or number, just returning those values;
;	If it names a register, returning the register value;
;	If it is *, returning the register value of *;
;	If it is a list, evaluating it as a Clojure form;
;	If it is bound to a variable, returning its value;
;	Otherwise, nil.
(defn evaluate-form
  [s-exp]
  (cond
    (number? s-exp) s-exp
    (string? s-exp) s-exp
    (internal-getr s-exp) (internal-getr s-exp)
    (= s-exp '*) (internal-getr '*)
    (seq? s-exp) (eval s-exp) ;; No need for this peval thing.
    (resolve s-exp) (resolve s-exp)
    :default nil))

(defmacro evaluate-form-in-context [config form]
  `(let [*config* ~config]
     (binding [*config* *config*])
     (evaluate-form ~form)))

(defmacro evaluate-form-in-context* [config form]
  `(let [*config* ~config
         registers (get-registers ~config)
         res nil]
     (binding [*config* *config*])
     (internal-setr '* (first (get-string)))
     (set! res (evaluate-form ~form))
     (set-registers registers)
     res))

(defmacro setr [register & forms]
  `(set-registers (assoc (get-registers)                    ;;map
                    '~register                         ;;key
                    (setup-forms (map evaluate-form '~forms))))) ;;val(s)

(defmacro setrq [register & forms]
  `(set-registers (assoc (get-registers)                    ;;map
                    '~register                               ;;key
                    (quote ~forms))))           ;;val(s)

(defmacro addr [register & forms]
  `(if ((keyword '~register)(get-registers))
     (let [contents# ((keyword '~register)(get-registers))
           forms# (map evaluate-form '~forms)
           newcontents# (conj forms# contents#)]
       (swap! (get-registers) assoc (keyword '~register) newcontents#))
     (setr ~register ~forms)
     )
  )

(defmacro addl [register & forms]
  `(if ((keyword '~register)(get-registers))
     (let [contents# ((keyword '~register)(get-registers))
           forms# (map evaluate-form ~forms)
           newcontents# (cons forms# contents#)]
       (swap! (get-registers) assoc (keyword '~register) newcontents#))
     (setr ~register ~forms)
     )
  )

(defmacro sendr [register & forms]
  (if forms
     `(set-sendr-acts (assoc (get-sendr-acts)               ;map
                        '~register                           ;key
                        (setup-forms (map evaluate-form '~forms)))) ;val(s)
     `(set-sendr-acts (assoc (get-sendr-acts)               ;map
                        '~register                          ;key
                        (internal-getr ~register)))))       ;val(s)

(defmacro sendrq [register & forms]
  (if forms
    `(set-sendr-acts (assoc (get-sendr-acts)               ;map
                       '~register                           ;key
                       (quote ~forms))) ;val(s)
    `(set-sendr-acts (assoc (get-sendr-acts)               ;map
                       '~register                          ;key
                       (internal-getr ~register)))))

(defmacro liftr [register & forms]
  (if forms
    `(set-liftr-acts (assoc (get-liftr-acts)               ;map
                       '~register                           ;key
                       (setup-forms (map evaluate-form '~forms)))) ;val(s)
    `(set-liftr-acts (assoc (get-liftr-acts)               ;map
                       '~register                          ;key
                       (internal-getr ~register)))))       ;val(s)

(defmacro hold [cat form]
  `(set-hold-register (assoc (get-hold-register)
                          '~cat
                          '~form)))
(defn any-holds? []
  (binding [*config* *config*]
    (let [holdr (get-hold-register)]
      (if (some? holdr)
        false
        true))))

(defn test-holdr []
  (binding [*config* (make-configuration)                   ;; current configuration we're modifying.
            *parse-trees* nil]
    (set-hold-register '{n "dog"})
    (hold v "ran")
    (get-hold-register)
    (any-holds?)))

(defmacro to [state & forms]
  `(if ~forms
     (let [temp '(map evaluate-form ~forms)
           (binding [*config* *config*])
           (if temp
             (set-string
               (cons temp (rest (get-string)))))
           (set-string (rest (get-string)))
           '(~state)])
     `(do (set-string (rest (get-string))) '~state)))

(defmacro jump [state]
  ` ~state)

(defmacro getf [feature]
  `((keyword '~feature) *lexicon*))


;(defn internal-setr [register & forms]
;  (do
;    (with-local-vars (binding [*config* *config*]))
;    (set-registers (cons
;                     (cons register (setup-forms forms))
;                         ()))))

;(defn evaluate-actions [acts]
;  (binding [*config* *config*])
;  ())

(defmacro buildq
  "Handles the ATN 'buildq' directive."
  [& buildargs]
  `(binding [*frags* (rest '~buildargs)]
     (bldq (first '~buildargs))))

(defn bldq
  "Does the instantiation of registers and special atoms required by BUILD."
  [exp]
  (println exp)
  (cond
    ;; If exp is +, evaluate the first form in frags, then remove it from that list.
    (= exp '+)
    (let [res (evaluate-form (first *frags*))]
      (evaluate-form(first *frags*))
      ;(println (evaluate-form(first *frags*)))
      (set! *frags* (rest *frags*))
      res)
    ;; If exp is *, just get its value.
    (= exp '*)
    (internal-getr '*)
    (and (seq? exp) (= (first exp) (symbol "@")))
    (apply concat (bldq (rest exp)))
    ;; Atomic things just stay themselves.
    (or (symbol? exp) (char? exp) (number? exp) (string? exp) (empty? exp))
    exp
    ;; Otherwise...
    :default
    (cons (bldq (first exp)) (bldq (rest exp)))))

;;tests on the arcs
(defmacro nullr
  "Checks if the register is empty"
  [register]
  `(= ((keyword '~register) (get-registers)) nil))

(defmacro end-of-sentence
  "Returns true when at the end of a sentence or last punctuation"
  []
  `(binding [*config* *config*])
  (or (= (:string *config*) nil)
      (and (some (partial = (first (:string *config*))) punctuation)
           (= (rest (:string *config*)) nil))))

;;DO LATER/WHEN IT COMES UP
;;(defmacro checkf [feature value])
;;(defmacro catcheck [word cat])
;;(defmacro x-agree [form form])
;;(defmacro x-start [])

(defn do-acts [pop-result *config* push-config acts]
  (binding [*config* *config*])
  (let [push-type (first (first acts))
        register (second (first acts))
        old-star-reg (rest (rest (first acts)))]
    (set-registers (get-registers push-config))
    (eval-liftr (get-liftr-acts))
    (set-level (get-level push-config))
    (set-pop-config (get-pop-config push-config))
    (set-sendr-acts (get-sendr-actions push-config))
    (set-liftr-acts (get-liftr-acts push-config))
    (internal-setr register pop-result)
    (set! acts (rest acts))
    (case push-type
      (push
        (set-string (cons pop-result (get-string))))
      (call
        (if (= register '*)
          (set-string (cons pop-result (get-string)))
          (set-string (cons (first (get-string push-config)) (get-string)))))
      (rcall
        (if (= register '*)
          (set-string (cons pop-result (get-string push-config)))
          (set-string (get-string push-config)))))
    (if (and (get push-type '(call recall))
             (not (= register '*)))
      (internal-setr '* old-star-reg))
    (set-state (first (last (evaluate-actions acts))))
    *config*))

(defn convertline [sentence]
  (str/split sentence #"\s+")
  )

(defn is-uppercase? [char]
  (#(Character/isUpperCase char)))

;(defun flat* (l)
;       "Flattens a list so that all atoms are at the top level."
;       (cond ((null l) nil)
;             ((atom l) (list l))
;             ((sneps::is.n l) (list l))
;             (t (nconc (flat* (car l))
;                       (flat* (cdr l))))
;             ))
;
;(defun flatten (s)
;       "Flattens its argument list. I. e., makes all components top-level, and returns the list
;       if it contains at least two elements, its 'car' otherwise."
;       (cond ((cdr (setq s (flat* s))) s)
;             (t (car s))
;             ))

(defn internal-parse [coll-sentence]
  (let [cfg (make-configuration
              :state to-state
              :string string
              :registers nil
              :hold-register nil
              :pop-config (make-configuration
                            :state "end of parse"
                            :string nil
                            :registers nil
                            :hold-register nil
                            :pop-config nil
                            :level 0)
              :level 1)]
    (binding [*config* *config*])
    (set! *config* (mainloop cfg))
    (cond (*all-parses*
            ()

            ))))

(defn nl-tell [sentence]
  "Given a string sentence. Invokes the parser and returns a string result"
  (let [*config* (make-configuration)
        converted-sentence (convertline sentence)]
    (if (nil? *lexicon*)
      (println "NO LEXICON LOADED"))
    (if (nil? *atn-arcs-ht*)
      (println "NO GRAMMAR LOADED"))

    (reset! global-state 's)
    (reset! trace-level 0)
    (reset! *parse-trees* nil)
    (reset! *all-parses* nil)

    ;;Names and other uppercase cases (can take care of later)
    ;(if (and (is-uppercase?
    ;           (first (first converted-sentence)))))
    (set! converted-sentence
          (cons (lower-case (first converted-sentence))
                (rest converted-sentence)))
    (internal-parse converted-sentence)))

(defn parse [& args]
  (let [*config* (make-configuration)
        sentence global-state]
    (if (nil? *lexicon*)
      (println "NO LEXICON LOADED"))
    (if (nil? *atn-arcs-ht*)
      (println "NO GRAMMAR LOADED"))

    (reset! global-state 's)
    (reset! trace-level 0)

    (loop [arg args]
      (if (number? arg)
        (reset! trace-level arg))
      (if (atom? arg)
        (reset! global-state arg)))

    (reset! *parse-trees* nil)
    (reset! *all-parses* nil)

    ))

;;MAINLOOP
(defn mainloop [entry]
  (binding [*config* *config*])
  (let [arcs (getarcs (get-state entry))
        sendrs (get-sendr-acts entry)
        any-input-left? (get-string entry)
        all-configs nil]
    (loop [arc arcs]
      (case (first arc)

        (jump
          (if (and (any-input-left?) (evaluate-form-in-context* entry (jump-test arc)))
            (let [*config* entry]
              (internal-setr '* (first (get-string)))
              (evaluate-actions (jump-actions arc))
              (set-state (jump-state arc))
              *config*)
            (set! all-configs (conj all-configs (mainloop *config*)))))
        (to
          (if (and (any-input-left?) (evaluate-form-in-context* entry (to-test arc)))
            (let [*config* entry]
              (internal-setr '* (first (get-string)))
              (evalute-actions (to-actions arc))
              (set-string (rest (get-string)))
              (if (to-form arc)
                (set-string (conj (list (evaluate-form (to-form arc))) (get-string))))
              (set-state (to-state arc))
              (set! all-configs (conj all-configs (mainloop *config*))))))
        (tst
          (if (and (any-input-left?) (evaluate-form-in-context* entry (to-test arc)))
            (let [*config* entry]
              (internal-setr '* (first (get-string)))
              (set-state (first (last (evaluate-actions (tst-actions arc)))))
              (set! all-configs (conj all-configs (mainloop *config*))))))
        (pop
          (if (evaluate-form-in-context* entry (pop-test arc))
            (let [*config* entry
                  string (get-string)
                  liftrs nil
                  presult nil]
              (set! liftrs (get-liftr-acts))
              (if (and (= (get-level) 1)
                       (nil? (get-string))
                       (nil? (get-hold-register)))
                (do
                  (evaluate-actions (pop-actions arc))
                  (set! presult (evaluate-form (pop-form arc)))
                  (set! *config* (get-pop-config))
                  (internal-setr '* presult)
                  (set-string string)
                  (set-liftr-acts liftrs)
                  (set! all-configs (conj all-configs (list *config*)))
                  (if (and (presult) (= trace-level 0))
                    (print-parse presult))
                  (if (and (not (any-holds?)) (not (= (get-level) 1)))
                    (do
                      (evaluate-actions (pop-actions arc))
                      (set! presult (evaluate-form (pop-form arc)))
                      (set! all-configs (conj (all-configs
                                                (mainloop (do-acts presult *config*
                                                                   (get-pop-config)
                                                                   (get-push-acts)))))))))))))
        (push
          (if (and (any-input-left?) (evaluate-form-in-context* entry (push-test arc)))
            (let [*config* entry
                  acts nil]
              (internal-setr '* (first (get-string)))
              ;(set! acts)
              (set-state (push-state arc))
              (set-level (+ 1 (get-level)))
              (set-pop-config entry)
              (set-push-acts acts)
              (set-registers nil)
              (set-liftr-acts nil)
              (set! all-configs (conj all-configs (mainloop *config*))))))
        (call
          (if (and (any-input-left?) (evaluate-form-in-context* entry (call-test arc)))
            (let [*config* entry
                  acts nil]
              (internal-setr '* (first (get-string)))
              ;(set! acts)
              (set-string (conj (list (evaluate-form (call-form arc)))
                                (rest (get-string))))
              (set-state (call-state arc))
              (set-level (+ 1 (get-level)))
              (set-pop-config entry)
              (set-push-acts acts)
              (set-liftr-acts nil)
              (set-registers nil)
              (set! all-configs (conj all-configs (mainloop *config*))))))
        (rcall
          (if (and (any-input-left?) (evaluate-form-in-context* entry (rcall-test arc)))
            (let [*config* entry
                  acts nil]
              (internal-setr '* (first (get-string)))
              ;(set! acts)
              (set-string (conj (list (evaluate-form (rcall-form arc)))))
              (set-state (rcall-state arc))
              (set-level (+ 1 (get-level)))
              (set-pop-config entry)
              (set-push-acts acts)
              (set-liftr-acts nil)
              (set-registers nil)
              (set! all-configs (conj all-configs (mainloop *config*))))))
        (vir
          (if (and (evaluate-form-in-context* entry (vir-test arc))
                   (get (vir-constit-type arc) (get-hold-register entry)))
            (let [*config* entry]
              (loop [hold-item (get-hold-register)]
                (set-string (cons (first (rest (hold-item))) (get-string)))
                (set-hold-register (remove hold-item (get-hold-register)))
                (internal-setr '* (first (rest (hold-item))))
                (set-state (first (last (evaluate-actions (vir-actions arc)))))
                (set! all-configs (conj all-configs (mainloop *config*)))))))
        (cat
          (if (any-input-left?)
            (let [wrd-senses (get-senses (cat-category arc) (get-string entry))]
              (if wrd-senses
                (do
                  (loop [trans wrd-senses]
                    (let [*lexicon* (first (rest trans))
                          len (first trans)
                          fword (first (rest (rest trans)))
                          *config* entry]
                      (binding [*lexicon* *lexicon*])
                      (internal-setr '* (or (internal-getf 'root) fword))
                      (set-string (cons (internal-getr '*) (nth (get-string) len)))
                      (if (not (evaluate-form (cat-test arc)))
                        (do
                          (set-state (first (last (evaluate-actions (cat-actions arc)))))
                          (set! *lexicon* nil)
                          (set! all-configs (conj all-configs (mainloop *config*)))))
                      (set! *lexicon* nil))))))))
        (wrd
          (if (and (any-input-left?) (evaluate-form-in-context* entry (wrd-test arc)))
            (if (get (first (get-string entry)) (list (wrd-word arc)))
              (let [*config* entry]
                (internal-setr '* (first (get-string)))
                (set-state (first (last (evaluate-actions (wrd-actions arc)))))
                (set! all-configs (conj all-configs (mainloop *config*)))))))
        ))
    all-configs))