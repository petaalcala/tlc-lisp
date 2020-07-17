(ns untitled1.core)


(declare evaluar)
(declare aplicar)
(declare controlar-aridad)
(declare igual?)
(declare cargar-arch)
(declare imprimir)
(declare actualizar-amb)
(declare revisar-f)
(declare revisar-lae)
(declare buscar)
(declare evaluar-cond)
(declare evaluar-secuencia-en-cond)


(defn actualizar-amb
  [amb-global clave valor]
  (let [pos (.indexOf amb-global clave)]
    (cond
      (= pos -1) (do (conj (conj amb-global valor) clave))
      true (apply list(assoc (vec amb-global) (+ 1 pos) valor))
      )
    )
)

(defn controlar-aridad
  [lis num]
   (let [amount (count lis)]
    (cond
      (< amount num) (list '*error* 'too-few-args)
      (> amount num) (list '*error* 'too-many-args)
      true num
    )
  )
)


(defn imprimir
  ([elem]
   (cond
     (seq? elem)
      (cond
        (igual? (first elem) '*error*) (imprimir elem elem)
        true (do (println elem) elem)
      )
     true (do (println elem) elem)
   )
  )
  ([lis orig]
   (if (igual? (first lis) nil)
     (do (println "") orig)
     (do (print (first lis) "") (imprimir (next lis) orig))
     )
   )
  )


(defn convert_to_compare
  [elem]
  (cond
    (= elem nil) "nil"
    (= elem "") "nil"
    (= elem '()) "nil"
    (= elem false) "nil"
    (= (clojure.string/lower-case (str elem)) "nil") "nil"
    (not (seq? elem)) (clojure.string/lower-case (str elem))
    true elem
    )
  )

(defn igual?
  [a b]
  ;(println "a:", a)
  ;(println "b:", b)
  (cond
    ;(and (= a nil) (= b '()) ) true
    (and (= (convert_to_compare a) (convert_to_compare b))) true
    ;(= (clojure.string/lower-case (str a)) (clojure.string/lower-case (str b))) true
    true (= a b)
  )
)

(defn buscar
  [elem lis]
  (let [pos (.indexOf lis elem)]
    (if (= pos -1)
      (list '*error* 'unbound-symbol elem)
      (nth lis (+ pos 1))
      )
    ))


(defn evaluar-secuencia-en-cond
  [lis amb-global amb-local]
  (last (map evaluar lis amb-global amb-local))
  )

(defn evaluar-listado
  [lis amb-global amb-local]

  (let [res (evaluar (ffirst lis) amb-global amb-local)]
    (
     do
     (println "RES: " res)
     (println "LIS " lis)
     (cond
      (igual? nil (first res)) (evaluar-listado (next lis) amb-global amb-local)
      true (list (evaluar-secuencia-en-cond (nfirst lis) amb-global amb-local))
      ))
    )
  )

(defn evaluar-cond
  [lis amb-global amb-local]

  (cond
    (nil? lis) (list nil amb-global)
    true (evaluar-listado lis amb-global amb-local)
  )
)

(defn revisar-f
  [lis]
  (if (not (seq? lis))
    nil
    (if (= '*error* (first lis))
     lis
     nil
     )
   )
  )

(defn revisar-lae
  [lis]
    (let [lis_revisada (remove nil? (map revisar-f lis))]
      (if (= () lis_revisada)
        nil
        lis_revisada
        )
      )
  )


; ### AUXILIARES ###

(defn nil_lista
  [elem]
  (if (nil? elem)
    (list)
    elem
    )
)


; #### IMPLEMENTACIONES ######

(defn tlc-lisp-cons
  [lae]

  (let [ari (controlar-aridad lae 2), first_param (nil_lista (first lae)), second_param (nil_lista (second lae))]
    (cond
      (seq? ari) ari
      (not (seq? second_param)) (list '*error* 'list 'expected second_param)
      (and (seq? first_param) (empty? first_param)) (cons nil second_param)
      true (cons first_param second_param)
    )
  )
)


(defn tlc-lisp-reverse
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond (seq? ari) ari
          (igual? (first lae) nil ) nil
          (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
          true (let [res (reverse (first lae))] (if (igual? res nil) nil res))
          ))
  )


(defn tlc-lisp-append
  [lae]
  (let [ari (controlar-aridad lae 2)]
    (cond
      (seq? ari) ari
      (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
      (not (seq? (second lae))) (list '*error* 'list 'expected (second lae))
      true (flatten (conj (first lae) (second lae)))
      ))
)

(defn tlc-lisp-equal
  [lae]
  (let [ari (controlar-aridad lae 2), first_param (nil_lista (first lae)), second_param (nil_lista (second lae))]
    (cond
      (seq? ari) ari
      (igual? first_param second_param) 't
      true nil
    )
  )
)

(defn tlc-lisp-eval
  [lae amb-global amb-local]

  (let [ari (controlar-aridad lae 1), first_param (nil_lista (first lae)) ]
    (cond
      (seq? ari) ari
      (igual? first_param nil) nil
      true (first (evaluar first_param amb-global amb-local))
      )
  )
)

(defn tlc-lisp-first
  [lae]
  (let [ari (controlar-aridad lae 1), first_param (nil_lista (first lae))]
    (cond
      (seq? ari) ari
      (igual? first_param nil) nil
      true (first first_param)

    )

  )
)

(defn tlc-lisp-ge
  [lae]

  (let [ari (controlar-aridad lae 2), first_param (nil_lista (first lae)), second_param (nil_lista (second lae))]

    (cond
      (seq? ari) ari
      (not (number? first_param)) (list '*error* 'number 'expected first_param)
      (not (number? second_param)) (list '*error* 'number 'expected second_param)
      (>= first_param second_param) 't
      true nil
    )
  )
)

(defn tlc-lisp-gt
  [lae]

  (let [ari (controlar-aridad lae 2), first_param (nil_lista (first lae)), second_param (nil_lista (second lae))]

    (cond
      (seq? ari) ari
      (not (number? first_param)) (list '*error* 'number 'expected first_param)
      (not (number? second_param)) (list '*error* 'number 'expected second_param)
      (> first_param second_param) ('t)
      true nil
      )
    )
  )

(defn tlc-lisp-lt
  [lae]

  (let [ari (controlar-aridad lae 2), first_param (nil_lista (first lae)), second_param (nil_lista (second lae))]

    (cond
      (seq? ari) ari
      (not (number? first_param)) (list '*error* 'number 'expected first_param)
      (not (number? second_param)) (list '*error* 'number 'expected second_param)
      (< first_param second_param) 't
      true nil
      )
    )
  )

(defn convertir_a_bool
  [param]
  (if (igual? nil param) nil 't)
)

(defn tlc-lisp-not
  [lae]
  (let [ari (controlar-aridad lae 1), first_param (first lae)]
    (cond
      (seq? ari) ari
      true (convertir_a_bool (not first_param))
    )
  )
)

(defn tlc-lisp-null
  [lae]
  (let [ari (controlar-aridad lae 1), first_param (first lae)]
    (cond
      (seq? ari) ari
      true (convertir_a_bool (igual? nil first_param))
    )
  )
)




; REPL (read–eval–print loop).
; Aridad 0: Muestra mensaje de bienvenida y se llama recursivamente con el ambiente inicial.
; Aridad 1: Muestra >>> y lee una expresion y la evalua.
; Si la 2da. posicion del resultado es nil, retorna true (caso base de la recursividad).
; Si no, imprime la 1ra. pos. del resultado y se llama recursivamente con la 2da. pos. del resultado.
(defn repl
  ([]
   (println "Interprete de TLC-LISP en Clojure")
   (println "Trabajo Practico de 75.14/95.48 - Lenguajes Formales 2020")
   (println "Inspirado en:")
   (println "TLC-LISP Version 1.51 for the IBM Personal Computer")
   (println "Copyright (c) 1982, 1983, 1984, 1985 The Lisp Company") (flush)
   (repl '(add add append append cond cond cons cons de de env env equal equal eval eval exit exit
               first first ge ge gt gt if if lambda lambda length length list list load load lt lt nil nil not not
               null null or or prin3 prin3 quote quote read read rest rest reverse reverse setq setq sub sub
               t t terpri terpri + add - sub)))
  ([amb]
   (println "AMB: ")
   (println amb)
   (print ">>> ") (flush)
   ;(print amb)
   (try (let [res (evaluar (read) amb nil)]
          (if (nil? (fnext res))
            true
            (do (println "RES: " res) (imprimir (first res)) (println "RES: " res) (repl (fnext res)))))
        (catch Exception e (println) (print "*error* ") (println (get (Throwable->map e) :cause)) (repl amb)))))

; Evalua una expresion usando los ambientes global y local. Siempre retorna una lista con un resultado y un ambiente.
; Si la evaluacion falla, el resultado es una lista con '*error* como primer elemento, por ejemplo: (list '*error* 'too-many-args) y el ambiente es el ambiente global.
; Si la expresion es un escalar numero o cadena, retorna la expresion y el ambiente global.
; Si la expresion es otro tipo de escalar, la busca (en los ambientes local y global) y retorna el valor y el ambiente global.
; Si la expresion es una secuencia nula, retorna nil y el ambiente global.
; Si el primer elemento de la expresion es '*error*, retorna la expresion y el ambiente global.
; Si el primer elemento de la expresion es una forma especial o una macro, valida los demas elementos y retorna el resultado y el (nuevo?) ambiente.
; Si no lo es, se trata de una funcion en posicion de operador (es una aplicacion de calculo lambda), por lo que se llama a la funcion aplicar,
; pasandole 4 argumentos: la evaluacion del primer elemento, una lista con las evaluaciones de los demas, el ambiente global y el ambiente local.
(defn evaluar [expre amb-global amb-local]
  (print "EXPRE: ")
  (println expre)

  ;(print "global: ")
  ;(println amb-global)
  ;
  ;(print "local: ")
  ;(println amb-local)

  (if (not (seq? expre))
    (if (or (number? expre) (string? expre)) (list expre amb-global) (list (buscar expre (concat amb-local amb-global)) amb-global))
    (cond (igual? expre nil) (list nil amb-global)
          (igual? (first expre) '*error*) (list expre amb-global)
          (igual? (first expre) 'exit) (if (< (count (next expre)) 1) (list nil nil) (list (list '*error* 'too-many-args) amb-global))
          (igual? (first expre) 'setq) (cond (< (count (next expre)) 2) (list (list '*error* 'list 'expected nil) amb-global)
                                             (igual? (fnext expre) nil) (list (list '*error* 'cannot-set nil) amb-global)
                                             (not (symbol? (fnext expre))) (list (list '*error* 'symbol 'expected (fnext expre)) amb-global)
                                             (= (count (next expre)) 2) (let [res (evaluar (first (nnext expre)) amb-global amb-local)]
                                                                          (list (first res) (actualizar-amb amb-global (fnext expre) (first res))))
                                             true (let [res (evaluar (first (nnext expre)) amb-global amb-local)]
                                                    (evaluar (cons 'setq (next (nnext expre))) (actualizar-amb amb-global (fnext expre) (first res)) amb-local)))
          (igual? (first expre) 'de) (cond (< (count (next expre)) 2) (list (list '*error* 'list 'expected nil) amb-global)
                                           (and (not (igual? (first (nnext expre)) nil)) (not (seq? (first (nnext expre))))) (list (list '*error* 'list 'expected (first (nnext expre))) amb-global)
                                           (igual? (fnext expre) nil) (list (list '*error* 'cannot-set nil) amb-global)
                                           (not (symbol? (fnext expre))) (list (list '*error* 'symbol 'expected (fnext expre)) amb-global)
                                           true (list (fnext expre) (actualizar-amb amb-global (fnext expre) (cons 'lambda (nnext expre)))))
          (igual? (first expre) 'quote) (list (if (igual? (fnext expre) nil) nil (fnext expre)) amb-global)
          (igual? (first expre) 'lambda) (cond (< (count (next expre)) 1) (list (list '*error* 'list 'expected nil) amb-global)
                                               (and (not (igual? (fnext expre) nil)) (not (seq? (fnext expre)))) (list (list '*error* 'list 'expected (fnext expre)) amb-global)
                                               true (list expre amb-global))
          (igual? (first expre) 'cond) (evaluar-cond (next expre) amb-global amb-local)
          true (aplicar (first (evaluar (first expre) amb-global amb-local)) (map (fn [x] (first (evaluar x amb-global amb-local))) (next expre)) amb-global amb-local))))

; Aplica una funcion a una lista de argumentos evaluados, usando los ambientes global y local. Siempre retorna una lista con un resultado y un ambiente.
; Si la aplicacion falla, el resultado es una lista con '*error* como primer elemento, por ejemplo: (list '*error* 'arg-wrong-type) y el ambiente es el ambiente global.
; Aridad 4: Recibe la func., la lista de args. evaluados y los ambs. global y local. Se llama recursivamente agregando 2 args.: la func. revisada y la lista de args. revisada.
; Aridad 6: Si la funcion revisada no es nil, se la retorna con el amb. global.
; Si la lista de args. evaluados revisada no es nil, se la retorna con el amb. global.
; Si no, en caso de que la func. sea escalar (predefinida o definida por el usuario), se devuelven el resultado de su aplicacion (controlando la aridad) y el ambiente global.
; Si la func. no es escalar, se valida que la cantidad de parametros y argumentos coincidan, y:
; en caso de que se trate de una func. lambda con un solo cuerpo, se la evalua usando el amb. global intacto y el local actualizado con los params. ligados a los args.,
; en caso de haber multiples cuerpos, se llama a aplicar recursivamente, pasando la funcion lambda sin el primer cuerpo, la lista de argumentos evaluados,
; el amb. global actualizado con la eval. del 1er. cuerpo (usando el amb. global intacto y el local actualizado con los params. ligados a los args.) y el amb. local intacto.
(defn aplicar
  ([f lae amb-global amb-local]
   (println "F:" f)
   (aplicar (revisar-f f) (revisar-lae lae) f lae amb-global amb-local))
  ([resu1 resu2 f lae amb-global amb-local]
   (println "Resu1 " resu1)
   (println "Resu2 " resu2)
   (println "F " f)
   (println "Lae " lae)
   (if (igual? f 'append) (println " ES APPEND!!!!"))
   (cond resu1 (list resu1 amb-global)
         resu2 (list resu2 amb-global)
         true  (if (not (seq? f))
                 (list (cond
                         (igual? f 'env) (if (> (count lae) 0)
                                           (list '*error* 'too-many-args)
                                           (concat amb-global amb-local))
                         (igual? f 'first) (let [ari (controlar-aridad lae 1)]
                                             (cond (seq? ari) ari
                                                   (igual? (first lae) nil) nil
                                                   (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
                                                   true (ffirst lae)))

                         (igual? f 'add) (if (< (count lae) 2)
                                           (list '*error* 'too-few-args)
                                           (try (reduce + lae)
                                                (catch Exception e (list '*error* 'number-expected))))

                         (igual? f 'append) (tlc-lisp-append lae)

                         (igual? f 'reverse) (tlc-lisp-reverse lae)

                         (igual? f 'cons) (tlc-lisp-cons lae)

                         (igual? f 'equal) (tlc-lisp-equal lae)

                         (igual? f 'eval) (tlc-lisp-eval lae amb-global amb-local)

                         (igual? f 'not) (tlc-lisp-not lae)

                         (igual? f 'null ) (tlc-lisp-null lae)


                         true (let [lamb (buscar f (concat amb-local amb-global))]
                                (cond (or (number? lamb) (igual? lamb 't) (igual? lamb nil)) (list '*error* 'non-applicable-type lamb)
                                      (or (number? f) (igual? f 't) (igual? f nil)) (list '*error* 'non-applicable-type f)
                                      (igual? (first lamb) '*error*) lamb
                                      true (aplicar lamb lae amb-global amb-local)))) amb-global)
                 (cond (< (count lae) (count (fnext f))) (list (list '*error* 'too-few-args) amb-global)
                       (> (count lae) (count (fnext f))) (list (list '*error* 'too-many-args) amb-global)
                       true (if (nil? (next (nnext f)))
                              (evaluar (first (nnext f)) amb-global (concat (reduce concat (map list (fnext f) lae)) amb-local))
                              (aplicar (cons 'lambda (cons (fnext f) (next (nnext f)))) lae (fnext (evaluar (first (nnext f)) amb-global (concat (reduce concat (map list (fnext f) lae)) amb-local))) amb-local)))))))

; Falta terminar de implementar las 2 funciones anteriores (aplicar y evaluar)

; Falta implementar las 9 funciones auxiliares (actualizar-amb, controlar-aridad, imprimir, buscar, etc.)

; Falta hacer que la carga del interprete en Clojure (tlc-lisp.clj) retorne true


; Carga el contenido de un archivo.
; Aridad 3: Recibe los ambientes global y local y el nombre de un archivo
; (literal como string o atomo, con o sin extension .lsp, o el simbolo ligado al nombre de un archivo en el ambiente), abre el archivo
; y lee un elemento de la entrada (si falla, imprime nil), lo evalua y llama recursivamente con el (nuevo?) amb., nil, la entrada y un arg. mas: el resultado de la evaluacion.
; Aridad 4: lee un elem. del archivo (si falla, imprime el ultimo resultado), lo evalua y llama recursivamente con el (nuevo?) amb., nil, la entrada y el resultado de la eval.
(defn cargar-arch
  ([amb-global amb-local arch]
   (let [nomb (first (evaluar arch amb-global amb-local))]
     (if (and (seq? nomb) (igual? (first nomb) '*error*))
       (do (imprimir nomb) amb-global)
       (let [nm (clojure.string/lower-case (str nomb))
             nom (if (and (> (count nm) 4) (clojure.string/ends-with? nm ".lsp")) nm (str nm ".lsp"))
             ret (try (with-open [in (java.io.PushbackReader. (clojure.java.io/reader nom))]
                        (binding [*read-eval* false] (try (let [res (evaluar (read in) amb-global nil)]
                                                            (cargar-arch (fnext res) nil in res))
                                                          (catch Exception e (imprimir nil) amb-global))))
                      (catch java.io.FileNotFoundException e (imprimir (list '*error* 'file-open-error 'file-not-found nom '1 'READ)) amb-global))]
         ret))))
  ([amb-global amb-local in res]
   (try (let [res (evaluar (read in) amb-global nil)] (cargar-arch (fnext res) nil in res))
        (catch Exception e (imprimir (first res)) amb-global))))