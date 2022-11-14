(ns db-inmutable.io
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]))

; guardar-a-disco :: List[Map[Long, Map[String, Long], Long]] -> String -> Bool
(defn guardar-a-disco [datos nombre-fichero]
  (cond (not (list? datos)) (println "El primer argumento de la funcion 'guardar-a-disco' debe ser una lista")
        (not (string? nombre-fichero)) (println "El segundo argumento de la funcion 'guardar-a-disco' debe ser una cadena")
        :else (with-open [w (io/writer nombre-fichero)]
                (json/write datos w))))

; recuperar-de-disco :: String -> List[Map[Long, Map[String, Long], Long]]
(defn recuperar-de-disco [nombre-fichero]
  (cond (not (string? nombre-fichero)) (println "El argumento de la funcion 'recuperar-de-disco' debe ser una cadena")
        :else (with-open [r (io/reader nombre-fichero)]
                (json/read r :key-fn keyword))))
