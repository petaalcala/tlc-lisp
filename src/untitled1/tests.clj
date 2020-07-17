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


(deftest test-buscar

  (is (= (untitled1.core/buscar '- '(+ add - sub)) 'sub))
  (is (= (untitled1.core/buscar 'doble '(+ add - sub)) '(*error* unbound-symbol doble)))

  )


(deftest test-evaluar-cond

  (is (= (untitled1.core/evaluar-cond nil '(equal equal setq setq) nil) '(nil (equal equal setq setq))))
  (is (= (untitled1.core/evaluar-cond '(((equal 'a 'b) (setq x 1))) '(equal equal first first) nil) '(nil (equal equal first first))))
  ;(is (= (untitled1.core/evaluar-cond '(((equal 'a 'b) (setq x 1))) '(equal equal first first) nil) '(nil (equal equal first first))))
  ;(is (= (untitled1.core/evaluar-cond '(((equal 'a 'b) (setq x 1))) '(equal equal first first) nil) '(nil (equal equal first first))))

  )

(run-tests)