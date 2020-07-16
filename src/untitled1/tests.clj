(ns untitled1.tests)


(require '[clojure.test :refer [is deftest run-tests]])


(load-file "src/untitled1/core.clj")

(deftest test-igual

  (is (= 4 4))

  )


(deftest test-controlar-aridad

  (is (= (untitled1.core/controlar-aridad '(a b c) 4) '(*error* too-few-args)))

  (is (= (untitled1.core/controlar-aridad '(a b c d) 4) 4))

  (is (= (untitled1.core/controlar-aridad '(a b c d e) 4) '(*error* too-many-args)))

)


(deftest test-imprimir

  (is (= (untitled1.core/imprimir 5) 5))
  (is (= (untitled1.core/imprimir 'a) 'a))
  ;(is (= (imprimir \space) '(\space)))
  (is (= (untitled1.core/imprimir '(hola "mundo")) '(hola "mundo")))
  (is (= (untitled1.core/imprimir '(*error* hola "mundo")) '(*error* hola "mundo")))



  )


(run-tests)