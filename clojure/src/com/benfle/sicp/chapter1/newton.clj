(ns com.benfle.sicp.chapter1.newton)

(defn square [x]
  (* x x))

(defn average [x y]
  (/ (+ x y) 2))

;; Newton's method for square root.

(defn sqrt [x]
  (letfn [(good-enough? [guess]
            (< (Math/abs (- (square guess) x))
               0.001))
          (improve [guess]
            {:post [(not= % guess)]}
            (average guess (/ x guess)))
          (sqrt-iter [guess]
            (if (good-enough? guess)
              guess
              (recur (improve guess))))]
    (sqrt-iter 1.0)))

;; Better good-enough? test

(defn sqrt-2 [x]
  (letfn [(good-enough? [guess]
            (< (Math/abs (- guess (improve guess)))
               0.001))
          (improve [guess]
            (average guess (/ x guess)))
          (sqrt-iter [guess]
            (if (good-enough? guess)
              guess
              (recur (improve guess))))]
    (sqrt-iter 1.0)))

;; Using fixed-point

(def tolerance 0.00001)

(defn fixed-point [f guess]
  (letfn [(close-enough? [v1 v2]
            (< (Math/abs (- v1 v2)) tolerance))
          (try* [guess]
            (let [next (f guess)]
              (if (close-enough? guess next)
                next
                (recur next))))]
    (try* guess)))

(defn average-damp [f]
  (fn [x]
    (average x (f x))))

(defn sqrt-3 [x]
  (fixed-point (average-damp #(/ x %)) 1.0))

;; Newton's method (generic)

(def dx 0.00001)

(defn deriv [f]
  (fn [x]
    {:post [(not (zero? %))]}
    (/ (- (f (+ x dx))
          (f x))
       dx)))

(defn newton-transform [f]
  (fn [x]
    (- x (/ (f x)
            ((deriv f) x)))))

(defn newtons-method [f guess]
  (fixed-point (newton-transform f) guess))

(defn sqrt-4 [x]
  (newtons-method #(- (square %) x) 1.0))

;; Iterative Improvement

(defn iterative-improve [good-enough? improve]
  (fn [guess]
    (if (good-enough? guess)
      guess
      (recur (improve guess)))))

(defn sqrt-5 [x]
  (letfn [(improve [guess]
            (average guess (/ x guess)))
          (good-enough? [guess]
            (< (Math/abs (- guess (improve guess)))
               0.001))]
    ((iterative-improve good-enough? improve) 1.0)))

;; Note: some of the functions fail for large numbers (precision issues)
(defn run []
  (let [functions [sqrt sqrt-2 sqrt-3 sqrt-4 sqrt-5]]
    (doall
     (for [v [0.00001 0.001 0.5 1.0 4.0 16.0 1000.0 9999800001.0]
           idx (range 5)
           :let [expected (Math/sqrt v)
                 sqrt (nth functions idx)
                 res (sqrt v)
                 error (* 100 (/ (Math/abs (- expected res)) expected))]]
       (println (format "(sqrt-%d %19.5f) -> %10.2f (Error: %3.0f %%)" idx v res error)))))
  'DONE)
