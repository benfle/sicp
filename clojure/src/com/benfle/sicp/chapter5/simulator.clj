(ns com.benfle.sicp.chapter5.simulator
  "A Register-Machine simulator."
  (:require [clojure.spec.alpha :as s]))

;; ----------------------------------------------------------------
;; Error reporting
;; ----------------------------------------------------------------

(defn error
  [& args]
  (throw (Exception. (apply str args))))

;; ----------------------------------------------------------------
;; Tracing
;; ----------------------------------------------------------------

(defn trace
  [& args]
  (apply println "TRACE" args))

(defprotocol Tracable
  (trace-on [tracable]
    "Turns the tracing on.")
  (trace-off [tracable]
    "Turns the tracing off."))

;; ----------------------------------------------------------------
;; Registers
;; ----------------------------------------------------------------

(defprotocol Register
  (set-contents! [register value]
    "Stores a value in the register.")
  (get-contents [register]
    "Returns the contents of the given register."))

(defn make-register
  "Creates a register that holds a value."
  [name]
  (let [contents (volatile! ::unassigned)
        trace? (volatile! false)]
    (reify
      Register
      (set-contents! [register value]
        (when @trace?
          (trace "register" name (format "old: %s, new: %s" @contents value)))
        (vreset! contents value))
      (get-contents [register]
        @contents)
      Tracable
      (trace-on [register]
        (vreset! trace? true))
      (trace-off [register]
        (vreset! trace? false)))))

;; ----------------------------------------------------------------
;; The stack
;; ----------------------------------------------------------------

(defprotocol Stack
  (initialize-stack [stack]
    "Initialize the stack.")
  (push-stack [stack value]
    "Push the value onto the stack.")
  (pop-stack [stack]
    "Pop the value from the stack and return it.")
  (print-stack-statistics [stack]
    "Print the stack statistics on *stdout*."))

(defn make-stack
  []
  (let [s (volatile! '())
        number-pushes (volatile! 0)
        max-depth (volatile! 0)
        current-depth (volatile! 0)]
    (reify Stack
      (initialize-stack [stack]
        (vreset! s '())
        (vreset! number-pushes 0)
        (vreset! max-depth 0)
        (vreset! current-depth 0))
      (push-stack [stack value]
        (vswap! s conj value)
        (vswap! number-pushes inc)
        (vswap! current-depth inc)
        (vreset! max-depth (max @max-depth @current-depth)))
      (pop-stack [stack]
        (if-not (seq @s)
          (error "Empty stack -- POP")
          (let [top (peek @s)]
            (vswap! s pop)
            (vswap! current-depth dec)
            top)))
      (print-stack-statistics [stack]
        (println "STATS total-pushes  =" @number-pushes)
        (println "STATS maximum-depth =" @max-depth)))))

;; ----------------------------------------------------------------
;; Register-Machine
;; ----------------------------------------------------------------

(defrecord RegisterMachine [stack instructions labels operations registers instruction-count trace? pause-at-breakpoint?]
  Tracable
  (trace-on [machine]
    (vreset! trace? true))
  (trace-off [machine]
    (vreset! trace? false)))

(defn make-new-machine
  "Constructs and returns a new empty machine model."
  []
  (let [pc (make-register 'pc)
        flag (make-register 'flag)
        stack (make-stack)]
    (map->RegisterMachine
     {:stack stack
      :instructions (volatile! '())
      :labels (volatile! '{})
      :operations (volatile! {'initialize-stack #(initialize-stack stack)
                              'print-stack-statistics #(print-stack-statistics stack)})
      :registers (volatile! {'pc pc
                             'flag flag})
      :instruction-count (volatile! 0)
      :trace? (volatile! false)
      :pause-at-breakpoint? (volatile! true)})))

(declare assemble)

(defn install-instruction-sequence!
  "Installs the instructions in the machine."
  [machine insts]
  (vreset! (:instructions machine) insts))

(defn install-operations!
  "Installs the operations in the machine."
  [machine ops]
  (vswap! (:operations machine) merge ops))

(defn install-labels!
  "Installs the labels in the machine."
  [machine labels]
  (vreset! (:labels machine) labels))

(defn make-machine
  "Constructs and returns a model of the machine with the given operations and controller."
  [ops controller-text]
  (let [machine (make-new-machine)]
    (install-operations! machine ops)
    (install-instruction-sequence! machine (assemble controller-text machine))
    machine))

(defn operations
  [{:keys [operations]}]
  @operations)

(defn stack
  [{:keys [stack]}]
  stack)

(defn allocate-register!
  "Allocates a register with the given name in the machine."
  [{:keys [registers]} name]
  (if (contains? @registers name)
    (error "Multiply defined register: " name)
    (vswap! registers assoc name (make-register name))))

(defn get-register
  "Returns the register with the given name."
  [{:keys [registers]} name]
  (if-not (contains? @registers name)
    (error "Unknown register: " name)
    (get @registers name)))

(defn get-register-or-allocate!
  "Returns the register with the given name. Allocates it if necessary."
  [{:keys [registers] :as machine} name]
  (get (if (contains? @registers name)
         @registers
         (allocate-register! machine name))
       name))

(defn get-register-contents
  "Returns the contents of the register with the given name."
  [machine name]
  (get-contents (get-register machine name)))

(defn set-register-contents!
  "Sets the contents of the register with the given name."
  [machine name value]
  (set-contents! (get-register machine name) value))

(defn lookup-label
  "Returns the instructions associated to a given label in the machine."
  [{:keys [labels]} label]
  (if (contains? @labels label)
    (@labels label)
    (error "Undefined label -- ASSEMBLE: " label)))

(defn advance-pc!
  "Advance the program counter register of the machine."
  [machine]
  (let [pc (get-register machine 'pc)]
    (set-contents! pc (rest (get-contents pc)))))

(defn execute!
  "Executes all the installed instructions until the end."
  [{:keys [instruction-count trace? pause-at-breakpoint?] :as machine}]
  (loop []
    (let [insts (get-register-contents  machine 'pc)]
      (if-not (seq insts)
        :done
        (let [inst (first insts)]
          (if (and (:breakpoint? @inst) @pause-at-breakpoint?)
            (vreset! pause-at-breakpoint? false)
            (do
              (vreset! pause-at-breakpoint? true)
              (when @trace?
                (trace (:label @inst)
                       (s/unform ::instruction (:text @inst))))
              (vswap! instruction-count inc)
              ((:proc @inst))
              (recur))))))))

(defn start!
  "Simulates the execution of the given machine."
  [{:keys [instructions] :as machine}]
  (set-register-contents! machine 'pc @instructions)
  (execute! machine))

(defn instruction-count
  [{:keys [instruction-count]}]
  @instruction-count)

(defn reset-instruction-count
  "Resets the instruction count of the machine."
  [{:keys [instruction-count]}]
  (vreset! instruction-count 0))

(defn register-trace-on
  "Turns the trace on for the register with the given name."
  [machine name]
  (trace-on (get-register machine name)))

(defn register-trace-off
  "Turns the trace off for the register with the given name."
  [machine name]
  (trace-off (get-register machine name)))

(defn done?
  "Whether the machine is done."
  [machine]
  (not (seq (get-register-contents  machine 'pc))))

(defn instruction-at
  "The instruction at poistion `n` under the given `label`."
  [machine label n]
  (let [instructions (lookup-label machine label)]
    (if-let [instruction (nth instructions (dec n))]
      instruction
      (error "Unknown instruction at" label n))))

(defn set-breakpoint
  "Sets a breakpoint at the given instruction."
  [machine label n]
  (vswap! (instruction-at machine label n) assoc :breakpoint? true))

(defn proceed
  "Proceed after pausing at a breakpoint."
  [machine]
  (execute! machine))

(defn cancel-breakpoint
  "Cancels the given breakpoint."
  [machine label n]
  (vswap! (instruction-at machine label n) dissoc :breakpoint?))

(defn cancel-all-breakpoints
  "Cancels all breakpoints."
  [machine]
  (doseq [inst @(:instructions machine)]
    (vswap! inst dissoc :breakpoint?)))

;; ----------------------------------------------------------------
;; Assembler
;; ----------------------------------------------------------------

(defmulti make-execution-procedure
  "Generates the execution procedure for an instruction.

  Defines the *semantics* of each instruction of the register-machine language."
  (fn [[tag inst] machine] tag))

;; (const <constant-value>)

(s/def ::const (s/cat :tag #{'const} :value any?))

(defn make-const-exp
  "Returns the execution procedure to produce values for `const` expressions."
  [{:keys [value]}]
  (constantly value))

;; (label <label-name>)

(s/def ::label (s/cat :tag #{'label} :label symbol?))

(defn make-label-exp
  "Returns the execution procedure to produce values for `label` expressions."
  [{:keys [label]} machine]
  (constantly (lookup-label machine label)))

;; (reg <register-name>)

(s/def ::reg (s/cat :tag #{'reg} :reg symbol?))

(defn make-reg-exp
  "Returns the execution procedure to produce values for `reg` expressions."
  [{:keys [reg]} machine]
  (let [register (get-register-or-allocate! machine reg)]
    #(get-contents register)))

;; ((op <operation-name>) <input-1> ... <input-n>))

(s/def ::op (s/cat :op (s/spec
                        (s/cat :tag #{'op}
                               :name symbol?))
                   :operands (s/*
                              (s/spec
                               (s/alt :const ::const
                                      :reg ::reg)))))

(defn make-op-exp
  "Returns the execution procedure to produce values for `op` expressions."
  [{:keys [op operands]} machine]
  (let [operations (operations machine)]
    (if-not (contains? operations (:name op))
      (error "Unknown operation -- ASSEMBLE: " (:name op))
      (let [operation (get operations (:name op))
            aprocs (mapv #(case (first %)
                            :const (make-const-exp (second %))
                            :reg (make-reg-exp (second %) machine))
                         operands)]
        #(apply operation
                (map (fn [p] (p)) aprocs))))))

;; (assign <register-name> (const <constant-value>)
;; (assign <register-name> (label <label-name>)
;; (assign <register-name> (reg <register-name>)
;; (assign <register-name> (op <operation-name>) <input-1> ... <input-n>)

(s/def ::assign (s/cat :tag #{'assign}
                       :reg-name symbol?
                       :value-exp (s/alt :const (s/spec ::const)
                                         :label (s/spec ::label)
                                         :reg (s/spec ::reg)
                                         :op ::op)))

(defmethod make-execution-procedure :assign
  [[_ {:keys [reg-name value-exp]}] machine]
  (let [register (get-register-or-allocate! machine reg-name)
        [tag value] value-exp
        value-proc (case tag
                     :const (make-const-exp value)
                     :label (make-label-exp value machine)
                     :reg (make-reg-exp value machine)
                     :op (make-op-exp value machine))]
    (fn []
      (set-contents! register (value-proc))
      (advance-pc! machine))))

;; (test (op <operation-name>) <input-1> ... <input-n>)

(s/def ::test (s/cat :tag #{'test}
                     :condition ::op))

(defmethod make-execution-procedure :test
  [[_ {:keys [condition]}] machine]
  (let [condition-proc (make-op-exp condition machine)]
    (fn []
      (set-register-contents! machine 'flag (condition-proc))
      (advance-pc! machine))))

;; (branch (label <label-name>))

(s/def ::branch (s/cat :tag #{'branch}
                       :dest (s/spec ::label)))

(defmethod make-execution-procedure :branch
  [[_ {:keys [dest]}] machine]
  (let [insts (lookup-label machine (:label dest))]
    (fn []
      (if (get-register-contents machine 'flag)
        (set-register-contents! machine 'pc insts)
        (advance-pc! machine)))))

;; (goto (label <label-name>))
;; (goto (reg <register-name>))

(s/def ::goto (s/cat :tag #{'goto}
                     :dest (s/spec (s/or :label ::label
                                         :reg ::reg))))

(defmethod make-execution-procedure :goto
  [[_ {:keys [dest]}] machine]
  (let [[tag exp] dest]
    (case tag
      :label (let [insts (lookup-label machine (:label exp))]
               (fn []
                 (set-register-contents! machine 'pc insts)))
      :reg (let [reg (get-register-or-allocate! machine (:reg exp))]
             (fn []
               (set-register-contents! machine 'pc (get-contents reg)))))))

;; (save <register-name>)

(s/def ::save (s/cat :tag #{'save}
                     :reg-name symbol?))

(defmethod make-execution-procedure :save
  [[_ {:keys [reg-name]}] machine]
  (let [reg (get-register-or-allocate! machine reg-name)]
    (fn []
      (push-stack (stack machine) (get-contents reg))
      (advance-pc! machine))))

;; (restore <register-name>)

(s/def ::restore (s/cat :tag #{'restore}
                        :reg-name symbol?))

(defmethod make-execution-procedure :restore
  [[_ {:keys [reg-name]}] machine]
  (let [reg (get-register-or-allocate! machine reg-name)]
    (fn []
      (set-contents! reg (pop-stack (stack machine)))
      (advance-pc! machine))))

;; (perform (op <operation-name>) <input-1> ... <input-n>)

(s/def ::perform (s/cat :tag #{'perform}
                        :action ::op))

(defmethod make-execution-procedure :perform
  [[_ {:keys [action]}] machine]
  (let [action-proc (make-op-exp action machine)]
    (fn []
      (action-proc)
      (advance-pc! machine))))

;; instruction

(s/def ::instruction (s/or :assign ::assign
                           :test ::test
                           :branch ::branch
                           :goto ::goto
                           :save ::save
                           :restore ::restore
                           :perform ::perform))

(defn make-instruction
  [text]
  (volatile! {:text text}))

;; controller text

(s/def ::controller (s/coll-of (s/or :instruction ::instruction
                                     :label symbol?)))

(defn extract-labels
  [text]
  (if-not (seq text)
    {:instructions '()
     :labels {}}
    (let [{:keys [instructions labels]} (extract-labels (rest text))
          [tag value] (first text)]
      (if (= :label tag)
        (do
          (doseq [inst instructions]
            (when-not (:label @inst)
              (vswap! inst assoc :label value)))
          {:instructions instructions
           :labels (assoc labels value instructions)})
        {:instructions (conj instructions (make-instruction value))
         :labels labels}))))

(defn update-insts!
  "Generates the execution procedures for each instruction."
  [insts machine]
  (doseq [inst insts]
    (vswap! inst assoc :proc (make-execution-procedure (:text @inst) machine))))

(defn assemble
  [controller-text machine]
  (if-not (s/valid? ::controller controller-text)
    (error (s/explain-str ::controller controller-text))
    (let [{:keys [instructions labels]} (extract-labels (s/conform ::controller controller-text))]
      (install-labels! machine labels)
      (update-insts! instructions machine)
      instructions)))

;; ----------------------------------------------------------------
;; Examples
;; ----------------------------------------------------------------

(defn run
  "Run the machine with the given input registers's values and return the content of the output register."
  [machine input output]
  (doseq [[k v] input]
    (set-register-contents! machine k v))
  (start! machine)
  (get-register-contents machine output))

(defn gcd-machine
  []
  (make-machine
   {'mod mod
    '= =}
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op mod) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

(defn factorial-machine
  []
  (make-machine
   {'> >
    '* *
    '+ +}
   '((assign product (const 1))
     (assign counter (const 1))

     test-counter
     (test (op >) (reg counter) (reg n))
     (branch (label done))
     (assign product (op *) (reg product) (reg counter))
     (assign counter (op +) (reg counter) (const 1))
     (goto (label test-counter))

     done)))

(defn sqrt-machine
  []
  (make-machine
   {'* *
    '- -
    'abs #(Math/abs %)
    '< <
    '/ /
    'average (fn [& args] (/ (reduce + 0 args) (count args)))}
   '((assign guess (const 1.0))

     test-good-enough
     (assign temp (op *) (reg guess) (reg guess))
     (assign temp (op -) (reg temp) (reg x))
     (assign temp (op abs) (reg temp))
     (test (op <) (reg temp) (const 0.001))
     (branch (label done))
     (assign temp (op /) (reg x) (reg guess))
     (assign guess (op average) (reg guess) (reg temp))
     (goto (label test-good-enough))

     done)))

(defn rec-expt-machine
  []
  (make-machine
   {'= =
    '- -
    '* *}
   '((assign continue (label done))

     loop
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     (save continue)
     (assign continue (label after-expt))
     (assign n (op -) (reg n) (const 1))
     (goto (label loop))

     after-expt
     (assign val (op *) (reg b) (reg val))
     (restore continue)
     (goto (reg continue))

     base-case
     (assign val (const 1))
     (goto (reg continue))

     done)))

(defn iter-expt-machine
  []
  (make-machine
   {'- -
    '* *
    '= =}
   '((assign counter (reg n))
     (assign product (const 1))

     loop
     (test (op =) (reg counter) (const 0))
     (branch (label done))
     (assign counter (op -) (reg counter) (const 1))
     (assign product (op *) (reg b) (reg product))
     (goto (label loop))

     done)))

(defn factorial-stats-machine
  []
  (make-machine
   {'* *
    '- -
    '= =}
   '(stats-loop
     (test (op =) (reg range) (const 0))
     (branch (label done))
     (perform (op initialize-stack))
     (assign n (reg range))
     (assign continue (label fact-done))

     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))

     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))
     (goto (reg continue))

     base-case
     (assign val (const 1))
     (goto (reg continue))

     fact-done
     (perform (op print-stack-statistics))
     (assign range (op -) (reg range) (const 1))
     (goto (label stats-loop))

     done)))

(defn smoke-test-statistics
  []
  (let [m (factorial-stats-machine)]
    (register-trace-on m 'range)
    (run m {'range 7} 'range)
    (assert (= (instruction-count m) 317))))

(defn smoke-test-breakpoints
  []
  (let [machine (gcd-machine)]
    (set-breakpoint machine 'test-b 4)
    (set-register-contents! machine 'a 35)
    (set-register-contents! machine 'b 28)
    (start! machine)
    (while (not (done? machine))
      (println "Contents of the registers at breakpoint:"
               (format "a=%d, b=%d"
                       (get-register-contents machine 'a)
                       (get-register-contents machine 'b)))
      (proceed machine))
    (assert (= (get-register-contents machine 'a) 7))))

(defn smoke-test
  []
  (assert (= (run (gcd-machine)       {'a 35 'b 28} 'a)                   7))
  (assert (= (run (factorial-machine) {'n 15}       'product) 1307674368000))
  (assert (= (run (sqrt-machine)      {'x 100}      'guess)              10.000000000139897))
  (assert (= (run (rec-expt-machine)  {'n 2 'b 4}   'val)                16))
  (assert (= (run (iter-expt-machine) {'b 2 'n 4}   'product)            16))
  (smoke-test-statistics)
  (smoke-test-breakpoints)
  :ok)

(comment

  (require '[com.benfle.sicp.chapter5.simulator :as sim] :reload)

  (sim/smoke-test)

  )
