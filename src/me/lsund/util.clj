(ns me.lsund.util
  (:require [clojure.set :as set]))

(defn all [xs]
  (every? true? xs))

(defn any [xs]
  (some true? xs))

(defn const
  "Always return the first argument, discard the rest."
  [x & _]
  x)

(defn flip [f x y]
  (f y x))

(defn select-keys-with-nil
  "The result of `(select-keys m ks)` but if an element is present
   in ks but not in m, add it with value nil. Order is not necessarily
   retained.

   Example:
   (select-keys-with-nil m [:a :b :d])
    => {:b 2, :d nil, :a 1}"
  [m ks]
  (->> [nil]
       cycle
       (interleave (set/difference (set ks) (set (keys m))))
       (apply hash-map)
       (merge (select-keys m ks))))

(defmacro comp+ [& args]
  "Works like `comp` but allows you to specify the last argument as
  input parametr to the composed function."
  `((comp ~@(butlast args)) ~(last args)))

                                        ; taken from https://github.com/raynes/fs/blob/master/src/me/raynes/fs.clj
(defn glob->regex
  "takes a glob-format string and returns a regex."
  [s]
  (loop [stream s
         re ""
         curly-depth 0]
    (let [[c j] stream]
      (cond
        (nil? c) (re-pattern ; we add ^ and $ since we check only for file names
                  (str "^" (if (= \. (first s)) "" "(?=[^\\.])") re "$"))
        (= c \\) (recur (nnext stream) (str re c c) curly-depth)
        (= c \/) (recur (next stream) (str re (if (= \. j) c "/(?=[^\\.])"))
                        curly-depth)
        (= c \*) (recur (next stream) (str re "[^/]*") curly-depth)
        (= c \?) (recur (next stream) (str re "[^/]") curly-depth)
        (= c \{) (recur (next stream) (str re \() (inc curly-depth))
        (= c \}) (recur (next stream) (str re \)) (dec curly-depth))
        (and (= c \,) (< 0 curly-depth)) (recur (next stream) (str re \|)
                                                curly-depth)
        (#{\. \( \) \| \+ \^ \$ \@ \%} c) (recur (next stream) (str re \\ c)
                                                 curly-depth)
        :else (recur (next stream) (str re c) curly-depth)))))
