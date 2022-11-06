(ns db-inmutable.db)

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
