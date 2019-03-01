(ns me.lsund.routes
  (:require [clojure.edn :as edn]
            [me.lsund.util :refer [select-keys-with-nil]]
            [compojure.core :as compojure]))

(defmacro generate-routes [routes-file & xs#]
  (let [route-spec# (edn/read-string (slurp routes-file))]
    `(compojure/routes
      ~@(for [[method path args & body] xs#]
          (case method
            get-route `(compojure/GET ~(if (keyword path)
                                         (get (:get route-spec#) path)
                                         (get-in (:get route-spec#) path))
                                      request-map#
                                      (do
                                        ((fn [{:keys ~args}] ~@body)
                                         (select-keys-with-nil (:params request-map#)
                                                               ~(mapv keyword args)))))
            post-route `(compojure/POST ~(if (keyword path)
                                           (get (:post route-spec#) path)
                                           (get-in (:post route-spec#) path))
                                        request-map#
                                        ((fn [{:keys ~args}] ~@body)
                                         (select-keys-with-nil (:params request-map#)
                                                               ~(mapv keyword args)))))))))
