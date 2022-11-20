(ns db-inmutable.db
  (:require [db-inmutable.io :as io]))

; mover-elemento :: (Map) -> Long -> Long -> (Map)
(defn- mover-elemento [lista origen destino]
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
  (cond (not (map? elt)) (println "El primer argumento de la función 'esta-en' debe ser un diccionario.")
        (not (list? lista)) (println "El segundo argumento de la función 'esta-en' debe ser una lista.")
        (not (keyword? clave)) (println "El tercer argumento de la función 'esta-en' debe ser un keyword.")
        :else (if (= lista '())
                false
                (let [filtro (fn [elt-filter]
                               (-> (get elt-filter clave)
                                   (= elt)))]
                  (not (= (filter filtro lista) '()))))))

; insertar :: Map -> (Map) -> String -> (Map)
(defn insertar [nuevo-dato datos nombre-fichero]
  (cond (not (map? nuevo-dato)) (println "El primer parámetro de la función 'insertar' debe ser un diccionario.")
        (not (list? datos)) (println "El segundo parámetro de la función 'insertar' debe ser una lista.")
        (not (string? nombre-fichero)) (println "El tercer parámetro de la función 'insertar' debe ser una cadena.")
        (esta-en nuevo-dato datos :value) (println "El nuevo dato ya está en la lista de datos.")
        :else (let [nuevo-indice (if (= datos '())
                                   1
                                   (->> datos
                                        (reduce (fn [acc elt]
                                                  (max acc (get elt :id)))
                                                0)
                                        (inc)))
                    nuevo-elemento (-> {:id nuevo-indice,
                                        :rev 0
                                        :value nuevo-dato}
                                       (list)) 
                    nueva-lista (reverse (into '() (concat datos nuevo-elemento)))] 
                (io/guardar-a-disco nueva-lista nombre-fichero)
                nueva-lista)))

; actualizar :: Long -> Map -> (Map) -> String -> (Map)
(defn actualizar [id nuevo-dato datos nombre-fichero]
  (cond (not (number? id)) (println "El primer parámetro de la función 'actualizar' debe ser un número.")
        (not (map? nuevo-dato)) (println "El segundo parámetro de la función 'actualizar' debe ser un diccionario.")
        (not (list? datos)) (println "El tercer parámetro de la función 'actualizar' debe ser una lista.")
        (not (string? nombre-fichero)) (println "El cuarto parámetro de la función 'actualizar' debe ser una cadena.")
        :else (let [nueva-revision (->> datos
                                        (filter (fn [elt]
                                                  (= (get elt :id) id)))
                                        (reduce (fn [acc elt]
                                                  (max acc (get elt :rev)))
                                                0)
                                        (inc)) 
                    nuevo-elemento (list {:id id,
                                          :rev nueva-revision
                                          :value nuevo-dato})
                    nueva-lista (reverse (into '() (concat datos nuevo-elemento)))]
                (io/guardar-a-disco nueva-lista nombre-fichero)
                nueva-lista)))

; eliminar :: Long -> (Map) -> String -> (Map)
(defn eliminar [id datos nombre-fichero]
  (cond (not (number? id)) (println "El primer parámetro de la función 'eliminar' debe ser un número.")
        (not (list? datos)) (println "El segundo parámetro de la función 'eliminar' debe ser una lista.")
        (not (string? nombre-fichero)) (println "El tercer parámetro de la función 'eliminar' debe ser una cadena.")
        :else (actualizar id {} datos nombre-fichero)))