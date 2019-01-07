(ns me.lsund.util)

(defmacro .+ [& rest]
  `((comp ~@(butlast rest)) ~(last rest)))

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

(defn all [xs]
  (every? true? xs))

(defn any [xs]
  (some true? xs))
