(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn
  value-at
  [board coord]
  (get-in board coord))


(defn
  has-value?
  [board coord]
  (if (zero? (value-at board coord))
    false
    true))


(defn
  row-values
  [board [row _]]
  (set (get board row)))


(defn
  col-values
  [board [_ col]]
  (reduce #(conj % (get %2 col)) #{} board))


(defn
  coord-pairs
  [coords]
  (for [row coords
        col coords]
    (vector row col)))


(defn
  block-values
  [board [row col]]
  (let [upperleftcoords (into [] (map #(* (int (/ % 3)) 3) [row col]))
        [rows cols] (into [] (map #(range % (+ % 3)) upperleftcoords))
        block-coords (for [rowss rows
                           colss cols]
                       (vector rowss colss))]
    (set (map #(value-at board %) block-coords))))


(defn
  valid-values-for
  [board coord]
  (let [used-values (set/union (row-values board coord)
                                       (col-values board coord)
                                       (block-values board coord))]
    (if (zero? (value-at board coord))
      (set/difference all-values used-values)
      #{})))


(defn
  filled?
  [board]
  (let [all-numbers (set (apply concat board))]
  (if (contains? all-numbers 0)
    false
    true)))

(defn
  rows
  [board]
  (reduce #(conj % (row-values board [%2 0])) [] (range 9)))

(defn
  valid-rows?
  [board]
  (loop [rowz (rows board)]
    (cond
      (empty? rowz) true
      (empty? (set/difference all-values (first rowz))) (recur (rest rowz))
      :else false)))

(defn
  cols
  [board]
  (reduce #(conj % (col-values board [0 %2])) [] (range 9)))

(defn
  valid-cols?
  [board]
  (loop [colz (cols board)]
    (cond
      (empty? colz) true
      (empty? (set/difference all-values (first colz))) (recur (rest colz))
      :else false)))

(defn
  blocks
  [board]
  (let [list-block-coords (for [rows [0 3 6]
                                cols [0 3 6]]
                            (vector rows cols))]
  (reduce #(conj % (block-values board %2)) [] list-block-coords)))

(defn valid-blocks? [board]
  (loop [blockz (blocks board)]
    (cond
      (empty? blockz) true
      (empty? (set/difference all-values (first blockz))) (recur (rest blockz))
      :else false)))

(defn
  valid-solution?
  [board]
  (and (valid-blocks? board) (valid-cols? board) (valid-rows? board)))

(defn
  set-value-at
  [board coord new-value]
  (assoc-in board coord new-value))


(defn
  find-empty-point
  [board]
  (loop [row 0
         col 0]
    (cond
      (= 9 col) (recur (inc row) 0)
      (= 9 row) nil
      (zero? (value-at board [row col])) (vector row col)
      :else (recur row (inc col)))))

(defn
  solver-helper
  [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [focus-point (find-empty-point board)
          values-to-try (valid-values-for board focus-point)]
      (for [value values-to-try
            solution (solver-helper (set-value-at board focus-point value))]
        solution))))


(defn
  solve
  [board]
  (first (solver-helper board)))

















