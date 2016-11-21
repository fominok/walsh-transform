(ns walsh.limiters
  (:require [clojure.string :as string]))

(defn crop-only
  "Fit number in n bits"
  [x n]
  (let [absed (Math/abs x)
        bits (Long/toBinaryString absed)
        diff (inc (- (count bits) n))]
    (if (pos? diff)
      (let [shifted (bit-shift-right absed diff)]
        (if (pos? x)
          shifted
          (- shifted)))
      x)))

(defn crop-inc
  "Fit number in n bits incrementing first bit after cropping"
  [x n]
  (let [absed (Math/abs x)
        bits (Long/toBinaryString absed)
        diff (inc (- (count bits) n))]
    (if (pos? diff)
      (let [shifted (inc (bit-shift-right absed diff))]
        (if (pos? x)
          shifted
          (- shifted)))
      x)))

(defn crop-round
  "Fit number in n bits rounding last cropped"
  [x n]
  (let [absed (Math/abs x)
        bits (Long/toBinaryString absed)
        diff (inc (- (count bits) n))]
    (if (pos? diff)
      (let [semi-shifted (bit-shift-right absed (dec diff))
            shifted (bit-shift-right semi-shifted 1)
            shifted-rounded (if (odd? semi-shifted)
                              (inc shifted)
                              shifted)]
        (if (pos? x)
          shifted-rounded
          (- shifted-rounded)))
      x)))

(defn- binstr-to-int
  "Converts binary string to unsiged int"
  [bits]
  (loop [exp (dec (count bits)) acc 0 [x & xs] (string/split bits #"")]
    (let [newacc (+ acc (* (read-string x) (long (Math/pow 2 exp))))]
      (if (empty? xs)
        newacc
        (recur (dec exp) newacc xs)))))
