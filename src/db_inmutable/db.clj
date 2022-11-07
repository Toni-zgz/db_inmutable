(ns db-inmutable.db
  (:require [db-inmutable.io :as io]))

; reordenar-lista :: (Map) -> Long -> Long -> (Map)
(defn mover-elemento [lista origen destino]
  (let [nodo-origen (first (filter (fn [elt] (= (get elt :id) origen)) lista))
        lista-sin-nodo (filter (fn [elt] (not (= (get elt :id) origen))) lista)
        num-elems (count lista)]
    (loop [indice 1
           entrada lista-sin-nodo
           salida '()]
      (if (> indice num-elems)
        (reverse salida)
        (let [nodo-actual (first entrada)
              nodo-modificado (if (= indice destino)
                                (assoc nodo-origen :id indice)
                                (assoc nodo-actual :id indice))
              nuevo-indice (+ indice 1)
              nueva-entrada (if (= indice destino)
                              entrada
                              (rest entrada))
              nueva-salida (conj salida nodo-modificado)]
        (recur nuevo-indice nueva-entrada nueva-salida))))))

; esta-en :: Map -> List Map (Long Long Map (String Long)) -> Bool
(defn- esta-en [elt lista clave]
  (cond (not (map? elt)) (println "El primer argumento de la función esta-en debe ser un diccionario.")
        (not (list? lista)) (println "El segundo argumento de la función esta-en debe ser una lista.")
        (not (keyword? clave)) (println "El tercer argumento de la función esta-en debe ser un keyword.")
        :else (if (= lista '())
                false
                (let [filtro (fn [elt-filter]
                               (-> (get elt-filter clave)
                                   (= elt)))]
                  (not (= (filter filtro lista) '()))))))

; insertar :: Map -> (Map) -> String -> (Map)
(defn insertar [nuevo-dato datos nombre-fichero]
  (cond (not (map? nuevo-dato)) (println "El primer parámetro debe ser un diccionario.")
        (not (list? datos)) (println "El segundo parámetro debe ser una lista.")
        (not (string? nombre-fichero)) (println "El tercer parámetro debe ser una cadena.")
        (esta-en nuevo-dato datos :value) (println "El nuevo dato ya está en la lista de datos.")
        :else (let [ultimo-indice (if (= datos '())
                                    0
                                    (reduce (fn [acc elt]
                                              (max acc (get elt :id)))
                                            0
                                            datos))
                    nuevo-indice (inc ultimo-indice)
                    nuevo-elemento {:id nuevo-indice,
                                    :rev 0
                                    :value nuevo-dato}
                    array-antiguo (into [] datos)
                    nuevo-array (conj array-antiguo nuevo-elemento)
                    nueva-lista (reverse (into '() nuevo-array))]
                (io/guardar-a-disco nueva-lista nombre-fichero)
                nueva-lista)))