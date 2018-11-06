(ns me.lsund.util)

(defmacro .+ [& rest]
  `((comp ~@(butlast rest)) ~(last rest)))
