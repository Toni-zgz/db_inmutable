(ns db-inmutable.core-test
  (:require [clojure.test :as test]
            [db-inmutable.core :refer :all]
            [db-inmutable.io :as io]
            [db-inmutable.db :as db]))

(test/testing "Tests unitarios de db-inmutable/io"
  (let [entrada '({:id 1, :value {:nombre "Blanca", :edad 34}, :version 0}
                  {:id 2, :value {:nombre "Juan", :edad 22}, :version 0})
        nombre-fichero "test.fiabledb"]
    (io/guardar-a-disco entrada nombre-fichero)
    (let [salida (io/recuperar-de-disco nombre-fichero)]
      (test/is (= salida entrada)))))

(test/testing "Tests unitarios de db-inmutable/io"
  (let [lista1 '({:id 1 :idioma :ingles}
                 {:id 2 :idioma :frances}
                 {:id 3 :idioma :aleman}
                 {:id 4 :idioma :italiano}
                 {:id 5 :idioma :portugues}
                 {:id 6 :idioma :ruso})
        lista2 '({:id 1 :idioma :ingles}
                 {:id 2 :idioma :aleman}
                 {:id 3 :idioma :italiano}
                 {:id 4 :idioma :portugues}
                 {:id 5 :idioma :frances}
                 {:id 6 :idioma :ruso})
        lista3 '({:id 1 :idioma :ingles}
                 {:id 2 :idioma :aleman}
                 {:id 3 :idioma :italiano}
                 {:id 4 :idioma :portugues}
                 {:id 5 :idioma :ruso}
                 {:id 6 :idioma :frances})
        lista4 '()
        lista5 '({:id 1, :rev 0, :value {:nombre "Alvaro", :edad 35}})
        lista6 '({:id 1, :rev 0, :value {:nombre "Alvaro", :edad 35}}
                 {:id 2, :rev 0, :value {:nombre "Rosa", :edad 50}})]
    (test/is (= (db/mover-elemento lista1 2 5) lista2))
    (test/is (= (db/mover-elemento lista1 2 6) lista3))
    (test/is (= (db/insertar {:nombre "Alvaro", :edad 35} lista4 "test.fiabledb") lista5))
    (test/is (= (db/insertar {:nombre "Rosa", :edad 50} lista5 "test.fiabledb") lista6))))
