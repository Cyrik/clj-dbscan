(ns dbscan
  (:require [clojure.string :as str] [clojure.set :as set] [criterium.core :as crit]))

(defprotocol APos
  (score [p])
  (^int x [p])
  (^int y [p])
  (^int distance [p p2]))

(deftype Pos
  [^int x-cor, ^int y-cor,^int score, ^int hash]
  Object
  (equals [this other] (and (= x-cor (x other)) (= y-cor (y other)) (= score (:score other))))
  (hashCode [this] hash)
  (toString [this] (str "{:x " x-cor ", :y " y-cor " :score " score "}"))
  APos
  (score [this] score)
  (x [this] x-cor)
  (y [this] y-cor)
  (distance [this p2] 
              (let [x-dist (unchecked-subtract-int x-cor (x p2))
                    y-dist (unchecked-subtract-int y-cor (y p2))]
                (unchecked-add-int (unchecked-multiply-int x-dist x-dist) (unchecked-multiply-int y-dist y-dist)))))

(def ^:dynamic *score* 0)
(def ^:dynamic *size* 10)

(defn make-pos [x y score]
  (Pos. (int x) (int y) (int score) (hash {:x x :y y :score score})))

(defn to-map [s]
  (let [[x y light] (map #(Integer/parseInt %) (str/split (str/triml s) #"\s+"))]  
    (make-pos (int x) (int y) (int light))))

(defn parse-coors [file-name]
  (vec (map to-map (drop 1 (str/split-lines (slurp file-name))))))

(defn get-neighbors [p, eps, coors]
  (doall (filter #(<= (distance p %) eps) coors)))

(def coors (filter #(>= (score %) *score*)(parse-coors "C:\\bin\\lights.coors")))

(defn get-x-y [p size]
  [(int (/ (x p) size)) (int (/ (y p) size))])

(defn get-cluster [p coors size]
  (nth (nth coors (int (/ (x p) size))) (int (/ (y p) size))))

(defn calc-num-clusters [eps size]
  (+ 1 (int (- (/ eps size) 0.00001))))

(defn get-clusters-around [x y num-clusters coors]
  (apply concat 
         (mapcat 
           #(subvec % 
                    (max 0 (- y num-clusters)) 
                    (min (count (first coors)) (+ y num-clusters 1))) 
           (subvec coors 
                   (max 0 (- x num-clusters)) 
                   (min (count coors) (+ x num-clusters 1))))))

(defn get-neighbors2 [p, eps, coors, size, quad-eps]
  (let [num-clusters (calc-num-clusters eps size)
        [x y] (get-x-y p size)]
    (get-neighbors p quad-eps (get-clusters-around x y num-clusters coors))))

(defn generic-max 
  ([[a]] a)
  ([[a b & more] f]
    (reduce #(max %1 (f %2)) (max (f a) (f b)) more)))

(defn make-scaled-3d-vector [size outer-n inner-n]
  (vec (for [_ (range (/ (inc outer-n) size))]
         (transient (vec (repeat (/ (inc inner-n) size) []))))))

(defn add-to-scaled-3d-vector [xs ys p size]
  (assoc! xs (int (/ (x p) size)) 
          (assoc! ys 
                  (int (/ (y p) size)) 
                  (conj (nth ys (/ (y p) size)) p))))

(defn cluster [coors size]
  (let [xs (transient (make-scaled-3d-vector size, (generic-max coors #(x %)), (generic-max coors #(y %))))]
    (loop [coors coors
           xs xs]
      (if-not (empty? coors)
        (let [p (first coors)           
              y-vec (nth xs (/ (x p) size))]
          (recur (rest coors) (add-to-scaled-3d-vector xs y-vec p size)))
        (vec (map #(persistent! %) (persistent! xs))))))) 

(def coors2 (cluster coors *size*))

(defn make-cluster [p eps min-pts coors size coors-map quad-eps]
  (loop [cluster []
         p p
         stack []
         coors-map coors-map]
    (if p
      (if-not (get coors-map p)
        (let [neighbors (get-neighbors2 p eps coors size quad-eps)]
          (if (>= (count neighbors) min-pts)          
            (let [not-visited-neighbors (concat neighbors stack)]
              (recur (conj cluster p) (first not-visited-neighbors) (rest not-visited-neighbors) (conj! coors-map [p :clustered])))
            (recur (conj cluster p) (first stack) (rest stack) (conj! coors-map [p :clustered]))))
        (recur cluster (first stack) (rest stack) coors-map))
      [cluster coors-map])))

(defn dbscan [ eps min-pts coors size coors-clusters]
  (let [quad-eps (* eps eps)
        cnt (count coors)]
    (loop [coors coors
           coors-map (transient {})
           clusters []
           i 0]
      (if (= (mod i 1000) 0) (println "done " i " of " cnt " with " (count coors-map) " visited points"))
	    (if-not (empty? coors)
	      (if-not (coors-map (first coors))
	        (let [neighbors (get-neighbors2 (first coors) eps coors-clusters size quad-eps)]
	          (if-not (< (count neighbors) min-pts)
	            (let [[cluster new-coors-map] (make-cluster (first coors) eps min-pts coors-clusters size coors-map quad-eps)
	                  clusters (if (>= (count cluster) min-pts) (conj clusters cluster) clusters)]
	              (recur (rest coors) new-coors-map clusters (inc i)))
	            (recur (rest coors) coors-map clusters (inc i))))
           (recur (rest coors) coors-map clusters (inc i)))
	      clusters))))

(defn print-to-csv [clusters file-name]
  (spit file-name (str "x y cluster\n" (apply str (apply concat (map-indexed 
                                                                (fn [i cluster] 
                                                                  (doall (map #(str (x %) " " (y %) " " i \newline) 
                                                                              cluster))) 
                                                                clusters))))))

