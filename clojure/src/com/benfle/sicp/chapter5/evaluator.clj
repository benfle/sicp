(ns com.benfle.sicp.chapter5.evaluator
  "Explicit-Control Evaluator"
  (:require [com.benfle.sicp.chapter5.simulator :as simulator]))

(defn error
  [& args]
  (throw (Exception. (clojure.string/join " " args))))

;; Primitive data structure

;; dotted pair: ( car . cdr )
(defprotocol DottedPair
  (car [pair]
    "Returns the car of the dotted pair.")
  (cdr [pair]
    "Returns the cdr of the dotted pair.")
  (set-car! [pair value]
    "Stores the `value` in the car of the dotted pair.")
  (set-cdr! [pair value]
    "Stores the `value` in the cdr of the dotted pair."))

(defn pair?
  [object]
  (satisfies? DottedPair object))

(defn scheme-cons
  [car cdr]
  (let [store (volatile! [car cdr])]
    (reify
      DottedPair
      (car [pair]
        (first @store))
      (cdr [pair]
        (second @store))
      (set-car! [pair value]
        (vswap! store assoc 0 value)
        value)
      (set-cdr! [pair value]
        (vswap! store assoc 1 value)
        value)
      clojure.lang.Seqable
      (seq [pair]
        (let [[car cdr] @store]
          (cons car (cond
                      (pair? cdr) (seq cdr)
                      (nil? cdr)  nil
                      :else       (list cdr)))))
      Object
      (toString [pair]
        (let [[car cdr] @store]
          (format "( %s . %s )" car cdr))))))

(def cadr   (comp car cdr))
(def cddr   (comp cdr cdr))
(def caddr  (comp car cdr cdr))
(def caadr  (comp car car cdr))
(def cdadr  (comp cdr car cdr))
(def cdddr  (comp cdr cdr cdr))
(def cadddr (comp car cdr cdr cdr))

(defn scheme-list
  [& args]
  (if-not (seq args)
    nil
    (scheme-cons (first args) (apply scheme-list (rest args)))))

(defn scheme-append
  [list1 list2]
  (if (nil? list1)
    list2
    (scheme-cons (car list1) (scheme-append (cdr list1) list2))))

(defn scheme-map
  [f lst]
  (loop [lst lst
         new-lst (scheme-list)]
    (if (nil? lst)
      new-lst
      (recur (cdr lst)
             (scheme-cons (f (car lst)) new-lst)))))

(defn scheme-count
  [list]
  (loop [list list
         acc 0]
    (if (nil? list)
      acc
      (recur (cdr list) (inc acc)))))

(defn tagged-list?
  [v tag]
  (and (pair? v)
       (= (car v) tag)))

;; Environment

(defn make-frame
  [vars vals]
  (scheme-cons vars vals))

(defn frame-variables
  [frame]
  (car frame))

(defn frame-values
  [frame]
  (cdr frame))

(defn print-frame
  [frame]
  (clojure.pprint/pprint
   (zipmap (seq (frame-variables frame))
           (seq (frame-values frame)))))

(def the-empty-environment nil)

(defn first-frame
  [env]
  (car env))

(defn enclosing-environment
  [env]
  (cdr env))

(defn print-environment
  [env]
  (scheme-map (fn [frame]
                (println "--------")
                (print-frame frame))
              env))

(defn add-binding-to-frame!
  [var val frame]
  (set-car! frame (scheme-cons var (car frame)))
  (set-cdr! frame (scheme-cons val (cdr frame))))

(defn extend-environment
  [vars vals base-env]
  (if (= (scheme-count vars) (scheme-count vals))
    (scheme-cons (make-frame vars vals) base-env)
    (if (< (scheme-count vars) (scheme-count vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(defn lookup-variable-value
  [var env]
  (loop [env env]
    (if (= env the-empty-environment)
      (error "Unbound variable" var)
      (let [frame (first-frame env)
            value (loop [vars (frame-variables frame)
                         vals (frame-values frame)]
                    (cond
                      (nil? vars)        ::not-found
                      (= var (car vars)) (car vals)
                      :else              (recur (cdr vars) (cdr vals))))]
        (if (= ::not-found value)
          (recur (enclosing-environment env))
          value)))))

(defn set-variable-value!
  [var val env]
  (loop [env env]
    (if (= env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (let [frame (first-frame env)
            value (loop [vars (frame-variables frame)
                         vals (frame-variables frame)]
                    (cond
                      (nil? vars)        ::not-found
                      (= var (car vars)) (set-car! vals val)
                      :else              (recur (cdr vars) (cdr vals))))]
        (when (= ::not-found value)
          (recur (enclosing-environment env)))))))

(defn define-variable!
  [var val env]
  (let [frame (first-frame env)]
    (loop [vars (frame-variables frame)
           vals (frame-values frame)]
      (cond
        (nil? vars)        (add-binding-to-frame! var val frame)
        (= var (car vars)) (set-car! vals val)
        :else              (recur (cdr vars) (cdr vals))))))

(def primitive-procedures
  (scheme-list
   (scheme-list 'car car)
   (scheme-list 'cdr cdr)
   (scheme-list 'cons scheme-cons)
   (scheme-list 'null? nil?)
   (scheme-list '+ +)
   (scheme-list '- -)
   (scheme-list '* *)
   (scheme-list '/ /)
   (scheme-list '= =)))

(defn primitive-procedure-names
  []
  (scheme-map car primitive-procedures))

(defn primitive-procedure-objects
  []
  (scheme-map #(scheme-list 'primitive (cadr %))
              primitive-procedures))

(defn  setup-environment
  []
  (let [initial-env (extend-environment (primitive-procedure-names)
                                        (primitive-procedure-objects)
                                        the-empty-environment)]
    (define-variable! 'true  true  initial-env)
    (define-variable! 'false false initial-env)
    (define-variable! 'null  nil   initial-env)
    initial-env))

(def the-global-environment (setup-environment))

(defn get-global-environment
  []
  the-global-environment)

;; Primitive operations of the evaluator

(def self-evaluating?          #(or (number? %) (string? %)))
(def variable?                 symbol?)
(def quoted?                   #(tagged-list? % 'quote))
(def text-of-quotation         cadr)
(def assignment?               #(tagged-list? % 'set!))
(def assignment-variable       cadr)
(def assignment-value          caddr)
(def lambda?                   #(tagged-list? % 'lambda))
(def lambda-parameters         cadr)
(def lambda-body               cddr)
(def make-lambda               #(scheme-cons 'lambda (scheme-cons %1 %2)))
(def definition?               #(tagged-list? % 'define))
(def definition-variable       #(if (symbol? (cadr %)) (cadr %) (caadr %)))
(def definition-value          #(if (symbol? (cadr %)) (caddr %) (make-lambda (cdadr %) (cddr %))))
(def if?                       #(tagged-list? % 'if))
(def if-predicate              cadr)
(def if-consequent             caddr)
(def if-alternative            #(and (not (nil? (cdddr %))) (cadddr %)))
(def make-if                   (partial scheme-list 'if))
(def begin?                    #(tagged-list? % 'begin))
(def begin-actions             cdr)
(def last-exp?                 #(nil? (cdr %)))
(def first-exp                 car)
(def rest-exps                 cdr)
(def application?              pair?)
(def operator                  car)
(def operands                  cdr)
(def no-operands?              nil?)
(def first-operand             car)
(def rest-operands             cdr)
(def make-procedure            (partial scheme-list 'procedure))
(def compound-procedure?       #(tagged-list? % 'procedure))
(def procedure-parameters      cadr)
(def procedure-body            caddr)
(def procedure-environment     cadddr)
(def primitive-procedure?      #(tagged-list? % 'primitive))
(def primitive-implementation  cadr)
(def apply-primitive-procedure #(apply (primitive-implementation %1) %2))
(def get-global-environment    (constantly the-global-environment))
(def empty-arglist             (constantly nil))
(def adjoin-arg                #(scheme-append %2 (scheme-list %1)))
(def last-operand?             #(nil? (cdr %)))
(def prompt-for-input          #(println "\n\n" %))
(def announce-output           #(println "\n" %))

;; REPL

(defn clojure->scheme
  [value]
  (cond
    (list? value) (apply scheme-list (map clojure->scheme value))
    :else         value))

(defn read-edn
  []
  (clojure->scheme (clojure.edn/read)))

(defn user-print
  [object]
  (if (compound-procedure? object)
    (print ['compound-procedure
            (procedure-parameters object)
            (procedure-body object)
            '<procedure-env>])
    (print (str object))))

(def eceval-operations
  {'self-evaluating?          self-evaluating?
   'variable?                 variable?
   'quoted?                   quoted?
   'text-of-quotation         text-of-quotation
   'assignment?               assignment?
   'assignment-variable       assignment-variable
   'assignment-value          assignment-value
   'lambda?                   lambda?
   'lambda-parameters         lambda-parameters
   'lambda-body               lambda-body
   'definition?               definition?
   'definition-variable       definition-variable
   'definition-value          definition-value
   'if?                       if?
   'if-predicate              if-predicate
   'if-consequent             if-consequent
   'if-alternative            if-alternative
   'begin?                    begin?
   'begin-actions             begin-actions
   'last-exp?                 last-exp?
   'first-exp                 first-exp
   'rest-exps                 rest-exps
   'application?              application?
   'operator                  operator
   'operands                  operands
   'no-operands?              no-operands?
   'first-operand             first-operand
   'rest-operands             rest-operands
   'make-procedure            make-procedure
   'compound-procedure?       compound-procedure?
   'procedure-parameters      procedure-parameters
   'procedure-body            procedure-body
   'procedure-environment     procedure-environment
   'true?                     true?
   'false?                    false?
   'primitive-procedure?      primitive-procedure?
   'primitive-implementation  primitive-implementation
   'apply-primitive-procedure apply-primitive-procedure
   'empty-arglist             empty-arglist
   'adjoin-arg                adjoin-arg
   'last-operand?             last-operand?
   'get-global-environment    get-global-environment
   'lookup-variable-value     lookup-variable-value
   'set-variable-value!       set-variable-value!
   'define-variable!          define-variable!
   'extend-environment        extend-environment
   'prompt-for-input          prompt-for-input
   'announce-output           announce-output
   'read                      read-edn
   'user-print                user-print
   'print-environment         print-environment})

(def eceval
  (simulator/make-machine
   eceval-operations
   '(

     ;; REPL

     read-eval-print-loop
     (perform (op initialize-stack))
     (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))

     print-result
     (perform (op print-stack-statistics))
     (perform (op announce-output) (const ";;; EC-Eval value:"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))

     ;; error handling

     unknown-expression-type
     (assign val (const unknown-expression-type))
     (goto (label signal-error))

     unknown-procedure-type
     (assign val (const unknown-procedure-type))
     (goto (label signal-error))

     signal-error
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))

     ;; evaluates the expression specified by `exp` in the environment `env`

     eval-dispatch
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op variable?) (reg exp))
     (branch (label ev-variable))
     (test (op quoted?) (reg exp))
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp))
     (branch (label ev-assignment))
     (test (op definition?) (reg exp))
     (branch (label ev-definition))
     (test (op if?) (reg exp))
     (branch (label ev-if))
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type))

     ;; evaluate simple expressions

     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))

     ev-variable
     (assign val (op lookup-variable-value) (reg exp) (reg env))
     (goto (reg continue))

     ev-quoted
     (assign val (op text-of-quotation) (reg exp))
     (goto (reg continue))

     ev-lambda
     (assign unev (op lambda-parameters) (reg exp))
     (assign exp (op lambda-body) (reg exp))
     (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
     (goto (reg continue))

     ;; evaluate procedure applications

     ev-application
     (save continue)                                    ; where to go after the application is done
     (save env)                                         ; will be needed to eval arguments
     (assign unev (op operands) (reg exp))              ; retrieve arguments to
     (save unev)                                        ; save them while we eval operator
     (assign exp (op operator) (reg exp))               ; prepare operator expression to be evaled
     (assign continue (label ev-appl-did-operator))     ; where to go once operator expression has been evaled
     (goto (label eval-dispatch))                       ; go eval the operator expression

     ev-appl-did-operator                               ; operator has been evaled
     (restore unev)                                     ; restore arguments
     (restore env)                                      ; restore environment
     (assign arg1 (op empty-arglist))                   ; init list of evaled arguments
     (assign proc (reg val))                            ; the evaled operator
     (test (op no-operands?) (reg unev))                ; no operands?
     (branch (label apply-dispatch))                    ; -> just eval the operator
     (save proc)                                        ; save the operator while we eval the operands

     ev-appl-operand-loop                               ; eval operands one by one
     (save arg1)                                        ; save list of evaled arguments
     (assign exp (op first-operand) (reg unev))         ; pop first operand that will be evaled
     (test (op last-operand?) (reg unev))               ; last operand?
     (branch (label ev-appl-last-arg))                  ; -> goto special case
     (save env)                                         ; save env for next operand
     (save unev)                                        ; save list of operands
     (assign continue (label ev-appl-accumulate-arg))   ; where to go once operand is evaled
     (goto (label eval-dispatch))                       ; eval operand

     ev-appl-accumulate-arg
     (restore unev)                                     ; restore list of operands
     (restore env)                                      ; restore env to evaluate operand
     (restore arg1)                                     ; restore list of evaled args
     (assign arg1 (op adjoin-arg) (reg val) (reg arg1)) ; add evaled arg to list
     (assign unev (op rest-operands) (reg unev))        ; remove operand from list
     (goto (label ev-appl-operand-loop))                ; go eval next operand

     ev-appl-last-arg                                   ; "evlis tail recursion optimization": no need to save env/unev for last arg
     (assign continue (label ev-appl-accum-last-arg))   ; where to go once last operand is evaled
     (goto (label eval-dispatch))                       ; go eval last operand

     ev-appl-accum-last-arg
     (restore arg1)                                     ; restore list of evaled args
     (assign arg1 (op adjoin-arg) (reg val) (reg arg1)) ; add last arg to list
     (restore proc)                                     ; restore evaled operator
     (goto (label apply-dispatch))                      ; go eval application

     apply-dispatch
     (test (op primitive-procedure?) (reg proc))
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))
     (branch (label compound-apply))
     (goto (label unknown-procedure-type))

     primitive-apply
     (assign val (op apply-primitive-procedure) (reg proc) (reg arg1))
     (restore continue)
     (goto (reg continue))

     compound-apply
     (assign unev (op procedure-parameters) (reg proc))
     (assign env (op procedure-environment) (reg proc))
     (assign env (op extend-environment) (reg unev) (reg arg1) (reg env))
     (assign unev (op procedure-body) (reg proc))
     (goto (label ev-sequence))

     ev-begin
     (assign unev (op begin-actions) (reg exp))
     (save continue)
     (goto (label ev-sequence))

     ev-sequence                                        ; tail-recursion!
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue (label ev-sequence-continue))
     (goto (label eval-dispatch))

     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-sequence))

     ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))

     ;; Conditionals

     ev-if
     (save exp)
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     (goto (label eval-dispatch))

     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))

     ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))

     ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))

     ;; Assignments

     ev-assignment
     (assign unev (op assignment-variable) (reg exp))
     (save unev)
     (assign exp (op assignment-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-assignment-1))
     (goto (label eval-dispatch))

     ev-assignment-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))

     ;; Definitions

     ev-definition
     (assign unev (op definition-variable) (reg exp))
     (save unev)
     (assign exp (op definition-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     (goto (label eval-dispatch))

     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op define-variable!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue)))))

(comment

  (require '[com.benfle.sicp.chapter5.simulator :as simulator] :reload)
  (require '[com.benfle.sicp.chapter5.evaluator :as evaluator] :reload)

  (simulator/start! evaluator/eceval)

  )
