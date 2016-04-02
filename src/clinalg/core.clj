(ns clinalg.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def zero-vector-1-exception "Cannot normalize the zero vector")
(def zero-vector-2-exception "Cannot compute an angle with the zero vector")
(def zero-vector-3-exception "No unique parallel component to the zero vector")
(def zero-vector-4-exception "No unique orthogonal component to the zero vector")
(def zero-vector-5-exception "Cross product is only defined in two or three dimensions")

(defonce precision 1e30)



(defn pow [x n]
  (Math/pow x n))

(defn sqrt [x]
  (Math/sqrt x))


(def NaN (Math/acos 10))

(defn v+ [& vs]
  (vec (apply map + vs)))

(defn v- [& vs]
  (vec (apply map - vs)))

(defn v* [m v]
  (vec (map (partial * m) v)))

(defn magnitude [v]
  (sqrt (reduce + (map #(pow % 2) v))))

(defn normalized [v]
  (try (v* (/ 1 (magnitude v)) v)
       (catch Exception e
         (if (zero? (apply + v)) (throw (Exception. zero-vector-1-exception))
                                 (throw e)))))

(def u normalized)

(defn inner-product [v w]
  (reduce + (map * v w)))

(def dot-product inner-product)
(def dot inner-product)

(defn theta-rad [v w]
  (try
    (Math/acos (inner-product (normalized v) (normalized w)))
    (catch Exception e (if (= (.getMessage e) zero-vector-1-exception)
                         (throw (Exception. zero-vector-2-exception))
                         (throw e)))))

(defn theta-deg [v w]
  (/ (* (theta-rad v w) 180) Math/PI))

(defn theta
  ([v w] (theta v w false))
  ([v w degrees?] (if degrees? (theta-deg v w) (theta-rad v w))))

(theta (normalized [0 1]) (normalized [1 0]))

(dot [1 1] [1 1])

(defn vzero? [v]
  (reduce #(and %1 %2) (map zero? v)))

(defn orthogonal? [v w]
  (>= (/ 1 precision) (dot v w)))

(defn parallel? [v w]
  (or
    (vzero? v)
    (vzero? w)
    (reduce #(and %1 %2) (map #(>= (/ 1 precision) %) (map #(- (Math/abs %1) (Math/abs %2)) (normalized v) (normalized w))))))

(defn component-parallel [v b]
  (try (v* (dot v (normalized b)) (normalized b))
       (catch Exception e (if (= (.getMessage e) zero-vector-1-exception)
                            (throw (Exception. zero-vector-3-exception))
                            (throw e)))))

(defn component-orthogonal [v b]
  (try (v- v (component-parallel v b))
       (catch Exception e (if (= (.getMessage e) zero-vector-3-exception)
                            (throw (Exception. zero-vector-4-exception))
                            (throw e)))))

(defn cross-product [v w]
  (if (or (> (count v) 3) (> (count w) 3)
          (< (count v) 2) (< (count w) 2))
    (throw (Exception. zero-vector-5-exception))

    (let [[x1 y1 z1] v
          [x2 y2 z2] w
          z1 (or z1 0)
          z2 (or z2 0)]
      [(- (* y1 z2) (* y2 z1))
       (- (- (* x1 z2) (* x2 z1)))
       (- (* x1 y2) (* x2 y1))])))

(defn area-of-parallelogram [v w]
  (magnitude (cross-product v w)))

(defn area-of-triangle [v w]
  (/ (area-of-parallelogram v w) 2))


;(cross-product [8.462 7.893] [6.984])

;(area-of-parallelogram [-8.987 -9.838 5.031] [-4.268 -1.861 -8.866])

;(area-of-triangle [1.5 9.547 3.691] [-6.007 0.124 5.772])

;(area-of-parallelogram [5 3 -2] [-1 0 3])

;(component-parallel [1 1] [0 0])

;(let [v1 [3.009 -6.172 3.692 -2.51]
;      v2 [6.404 -9.144 2.759 8.718]]
;  (projection v1 v2))
;(component [-9.88 -3.264 -8.159] [-2.155 -9.353 -9.473])
;(projection [3.039 1.879] [0.825 2.036])

;(projection [4 4] [0 10])
;(component [4 4] [0 10])10

;; Tests
;;
;(orthogonal? [0 0] [1.1 1])
;
;
;(parallel? [-7.579 -7.88] [22.737 23.64])
;
;(parallel? [-2.029 9.97 4.172]
;     [-9.231 -6.639 -7.245])
;
;(parallel? [-2.328 -7.284 -1.214]
;             [-1.821 1.072 -2.94])
;
;(orthogonal? [1 2]
;           [0 0])
;
;(orthogonal? [-7.579 -7.88] [22.737 23.64])
;
;(orthogonal? [-2.029 9.97 4.172]
;           [-9.231 -6.639 -7.245])
;
;
;(orthogonal? [-2.328 -7.284 -1.214]
;           [-1.821 1.072 -2.94])




;(theta [0 2] [0 3])

;(parallel? [1 1] [-1 -1.1])
;




;(theta [1 5] [1 6] false)

;(dot-product [7.887 4.138] [-8.802 6.776])

;(dot-product
;  [-5.955 -4.904 -1.874]
;  [-4.496 -8.755 7.103])

;(theta-rad [3.183 -7.627] [-2.668 5.319])
;(theta-deg [7.35 0.221 5.188] [2.751 8.259 3.985])



;(normalized [0 0 0])

;(v+ [8.218 -9.341] [-1.129 2.111])
;(v- [7.119 8.215] [-8.223 0.878])
;(v* [1.671 -1.012 -0.318] 7.41)