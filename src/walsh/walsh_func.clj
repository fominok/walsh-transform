(ns walsh.walsh-func
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer :all]
            [clojure.string :as string]))

(defn log2 [x]
  (/ (Math/log x) (Math/log 2)))

(defn hadamard-matrix-seq
  "Generates Hadamard matrix 2^n sequence, where n->inf"
  ([] (hadamard-matrix-seq [[1]]))
  ([pmatrix] (lazy-seq (let [nmatrix (join-along 0
                                                 (join-along 1 pmatrix pmatrix)
                                                 (join-along 1 pmatrix (- pmatrix)))]
                         (cons nmatrix (hadamard-matrix-seq nmatrix))))))

(defn- dec-to-gray
  "Decimal to Gray's code binary string with fixed length"
  [x posn]
  (let [bin (Long/toBinaryString (bit-xor x (bit-shift-right x 1)))]
    (str (apply str (repeat (- posn (count bin)) "0")) bin)))

(defn- binstr-to-long-inv
  "Converts inversed binary string to Long"
  [s]
  (loop [exp 0 acc 0 [x & xs] (string/split s #"")]
    (let [newacc (+ acc (* (read-string x) (long (Math/pow 2 exp))))]
      (if (empty? xs)
        newacc
        (recur (inc exp) newacc xs)))))

(defn- walsh-access
  "Converts index of Hadamard matrix of order `Ord' row to Walsh index"
  [n ord]
  (binstr-to-long-inv (dec-to-gray n ord)))

(defn walsh-nth
  "Get nth Walsh function"
  [n ord]
  (map int (nth (nth (hadamard-matrix-seq) (dec ord)) (walsh-access n ord))))

(defn walsh-coeffs
  "Get Walsh coefficients for discrete signal"
  [signal]
  (let [len (count signal)
        logbase2 (int (Math/ceil (log2 len)))]
    (map #(int (div (mmul signal (walsh-nth % logbase2)) len)) (range len))))

(defn restore-signal
  "Get signal from Walsh coefficients"
  [coeffs]
  (let [len (count coeffs)
        logbase2 (int (Math/ceil (log2 len)))]
    (map #(int (mmul coeffs (walsh-nth % logbase2)) ) (range len))))

