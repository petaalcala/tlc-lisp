
(require '[clojure.test :refer [is deftest run-tests]])


(load-file "src/tlclisp/tlclisp.clj")


(deftest test-evaluar

  (is (= (evaluar '(setq r 3) '(+ add) nil) ) '(3 (+ add r 3)) )
  (is (= (evaluar '(de doble (x) (+ x x)) '(+ add) nil)) '(doble (+ add doble (lambda (x) (+ x x)))) )
  (is (= (evaluar '(+ 2 3) '(+ add) nil) ) '(5 (+ add)) )
  (is (= (evaluar '(+ 2 3) '(add add) nil)) '((*error* unbound-symbol +) '(add add)) )
  (is (= (evaluar '(doble 3) '(+ add doble (lambda (x) (+ x x))) nil)  '(6 (+ add doble (lambda (x) (+ x x)))) ))
  (is (= (evaluar '(doble r) '(+ add r 4 doble (lambda (x) (+ x x))) nil) ) '(8 (+ add r 4 doble (lambda (x) (+ x x)))) )
  (is (= (evaluar '((lambda (x) (+ x x)) 3) '(+ add) nil)) '(6 (+ add)) )

)



(deftest test-aplicar
  (is (= (aplicar 'cons '(a (b)) '(cons cons) nil) '((a b) (cons cons))))
  (is (= (aplicar 'add '(4 5) '(+ add r 5) nil) '(9 (+ add r 5))))
  (is (= (evaluar '(doble r) '(+ add r 4 doble (lambda (x) (+ x x))) nil) '(8 (+ add r 4 doble (lambda (x) (+ x x))))))
  (is (= (aplicar '(lambda (x) (+ x x)) '(4) '(+ add r 4 doble (lambda (x) (+ x x))) nil) '(8 (+ add r 4 doble (lambda (x) (+ x x))))))

)

(deftest test-controlar-aridad
  (is (= (controlar-aridad '(a b c) 4) '(*error* too-few-args)))
  (is (= (controlar-aridad '(a b c d) 4) 4))
  (is (= (controlar-aridad '(a b c d e) 4) '(*error* too-many-args)))
)


(deftest test-igual
  (is (= (igual? nil 'NIL) true))
  (is (= (igual? nil "NIL") true ))
  (is (= (igual? nil ()) true ))
  (is (= (igual? () 'NIL) true ))
)


  (deftest test-imprimir
  (is (= (imprimir 5) 5))
  (is (= (imprimir 'a) 'a))
  (is (= (imprimir \space) '\space))
  (is (= (imprimir '(hola "mundo")) '(hola "mundo")))
  (is (= (imprimir '(*error* hola "mundo")) '(*error* hola "mundo")))
  )


(deftest test-actualizar-ambiente
  (is (= (actualizar-amb '(+ add - sub) 'x 1) '(x 1 + add - sub)))
  (is (= (actualizar-amb '(+ add - sub x 1 y 2) 'x 3) '(+ add - sub x 3 y 2)))
)

(deftest test-revisar-f
  (is (= (revisar-f 'doble) nil))
  (is (= (revisar-f '(*error* too-few-args)) '(*error* too-few-args)))
)

(deftest test-revisarlae
  (is (= (revisar-lae '(1 add first)) nil))
  (is (= (revisar-lae '(1 add (*error* too-many-args) first)) '(*error* too-many-args) ) )
)


(deftest test-buscar
  (is (= (buscar '- '(+ add - sub)) 'sub))
  (is (= (buscar 'doble '(+ add - sub)) '(*error* unbound-symbol doble)))
  )

(deftest test-evaluar-cond
  (is (= (evaluar-cond nil '(equal equal setq setq) nil) '(nil (equal equal setq setq))))
  (is (= (evaluar-cond '(((equal 'a 'b) (setq x 1))) '(equal equal first first) nil) '(nil (equal equal first first))))
  (is (= (evaluar-cond '(((equal 'a 'b) (setq x 1)) ((equal 'a 'a) (setq y 2))) '(equal equal setq setq) nil) '(2 (y 2 equal equal setq setq))))
  (is (= (evaluar-cond '(((equal 'a 'b) (setq x 1)) ((equal 'a 'a) (setq y 2) (setq z 3))) '(equal equal setq setq) nil) '(3 (z 3 y 2 equal equal setq setq))))
  )

(deftest test-evaluar-secuencia-en-cond
  (is (= (evaluar-secuencia-en-cond '((setq y 2)) '(setq setq) nil) '(2 (y 2 setq setq))))
  (is (= (evaluar-secuencia-en-cond '((setq y 2) (setq z 3)) '(setq setq) nil) '(3 (z 3 y 2 setq setq)) ))
  )

(run-tests)