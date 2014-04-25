(ns ray-casting-algorithm.cast
  (:refer-clojure :exclude [cond]))

(defmacro cond 
    "A variation on cond which sports let bindings:
         (cond 
           (odd? a) 1
           :let [a (quot a 2)]
           (odd? a) 2
           :else 3)" 
    [& clauses]
    (when-let [[test expr & clauses] (seq clauses)]
          (if (= :let test)
                  `(let ~expr (cond ~@clauses))
                  `(if ~test ~expr (cond ~@clauses)))))

(defn require-swap
  ""
  [segment]
  (> (get-in segment [:a :y]) (get-in segment [:b :y])))

(defn swap [segment] 
     (assoc segment :b (segment :a) :a (segment :b)))

(defn arrange-points
  ""
  [segment]
  (if (require-swap segment)
    (swap segment)
    segment))

(defn check 
  ""
  [py ay by]
  (loop [py py]
    (if (or (= py ay) (= py by))
      (recur (+ py 0.001))
      py)))

(defn intersects
  ""
  [point segment]
  (let [clean-seg (arrange-points segment)
        ax (get-in clean-seg [:a :x])
        bx (get-in clean-seg [:b :x])
        ay (get-in clean-seg [:a :y])
        by (get-in clean-seg [:b :y])
        py (check (point :y) ay by)
        px (point :x)]

    (cond
      ;; point is above, below, or to the right of the rectangle
      ;; determined by segment; ray does not intesect the segment.
      (or (> px (max ax bx)) (> py (max ay by)) (< py (min ay by))) 0
     
      ;; point is to left of the rectangle; ray intersects segment 
      (< px (min ax bx)) 1

      ;; point is within the rectangle...
      :let [m-red (if (= ax bx) nil (/ (- by ay) (- bx ax)))
            m-blue (if (= px ax) nil (/ (- py ay) (- px ax)))]

      (nil? m-blue) 1
      (nil? m-red) 0
      (>= m-blue m-red) 1
      
      ;; point falls on a border - we won't consider this 'within' the polygon
      :else 0)))

(defn point-in-polygon
  ""
  [point polygon]
  (odd? (reduce + (map #(intersects point %) polygon))))
