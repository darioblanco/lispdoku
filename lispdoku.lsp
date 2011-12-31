;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; LISPDOKU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					 

;;; Crear un nuevo estado a partir del estado actual del sudoku
(defun make-ESTADO-sudoku (descubiertos tablero)
  (list descubiertos tablero) 
)

;;; Devolver el numero de casillas rellenadas correctamente
(defun ESTADO-descubiertos (estado)
  (first estado)
)

;;; Devolver el tablero del estado
(defun ESTADO-tablero (estado)
	(second estado)
)

;;; Modificar descubiertos
(defun ESTADO-modDesc (estado descubiertos)
	(setf (first estado) descubiertos)
)



;; Funciones especificas para la resolucion del problema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Modificar tablero
(defun ESTADO-modTab (estado tablero)
	(setf (second estado) tablero)
)

;;; Obtener un nuevo estado a partir de otro rellenando otra casilla del sudoku
(defun make-ESTADO-rellenar (numero estado)
	; Cargamos el tablero del estado
     (setq tablero (ESTADO-tablero estado)) 	 
	 ; Buscamos la primera posicion vacia del tablero, y colocamos los valores a fila y col
	 (cond
		( (not (null (member 0 (first tablero))))  (setq fila 0) (setq col (get-Cero-Lista (first tablero))) )
		( (not (null (member 0 (second tablero)))) (setq fila 1) (setq col (get-Cero-Lista (second tablero))) )		
		( (not (null (member 0 (third tablero))))  (setq fila 2) (setq col (get-Cero-Lista (third tablero))) ) 
		( (not (null (member 0 (fourth tablero)))) (setq fila 3) (setq col (get-Cero-Lista (fourth tablero))) ) 	
	)
	 ; Comprobamos si el numero es valido para esa posicion. Si lo es generara el nuevo estado, solo generara estados validos.
	 (cond
		( ( esValida fila col numero tablero)  
				(setq newEstado (copy-tree estado))
				(setq tablero (ESTADO-tablero newEstado))
				(setf (nth col (nth fila tablero)) numero)
				(setq descubiertos(ESTADO-descubiertos newEstado))
				(incf descubiertos)
				(ESTADO-modDesc newEstado descubiertos)
				(ESTADO-modTab newEstado tablero)
				newEstado
		)		
	)
)

;;; Funcion auxiliar de make-ESTADO-rellenar
;;; Devuelve el numero de columna de la primera casilla encontrada en una fila
(defun get-Cero-Lista(lista)
	(cond 
		((zerop (first lista)) 0)
		((zerop (second lista)) 1)
		((zerop (third lista)) 2)
		((zerop (fourth lista)) 3)
	)
)


;;; Coloca los numeros aleatoriamente
;;; Llamarla con (colocarNumeros numero estado-sudoku)
(defun colocarNumeros (nUbicar)
    ; Tablero inicial
	(setq tablero  '( (0 0 0 0)(0 0 0 0)(0 0 0 0)(0 0 0 0) ))
    (setq descubiertos 0)
	
	(setq estado-inicial (copy-tree (make-ESTADO-sudoku descubiertos tablero)))
	(setq tablero (ESTADO-tablero estado-inicial))
	(setq descubiertos(ESTADO-descubiertos estado-inicial))

  
  (dotimes (n nUbicar)
      ;;Obtenemos un numero aleatorio del 1 al 4 cuya posicion sea valida
	  (loop 
	    (setq numero (+ (random 4) 1) nColumna (random 4) nFila (random 4))
	    (if (esValida nFila nColumna numero tablero)
	      (return)
	    )
	  )	
	  (setf (nth nColumna (nth nFila tablero)) numero) 
      (incf descubiertos)	
   )
   ;;Creamos un nuevo estado  
	(ESTADO-modDesc estado-inicial descubiertos)
	(ESTADO-modTab estado-inicial tablero)
	(format t "~%SUDOKU INICIAL~%")
	(imprime estado-inicial)
	estado-inicial
)
	

;;; Imprimir el tablero
(defun imprime (estado)
  (dolist (fila (ESTADO-tablero estado))
    (format t "~%-----------------~%")
	(dolist (elemento fila)
	  (format t "| ~D " elemento)
	)
    (format t "|")
  )
  (format t "~%-----------------~%~%~%")
)

  
;;; Comprobar la validez de la posicion
(defun esValida (fila col numero tablero)
     
	(cond
		;; Comprobamos que la casilla esta vacia
		((not(vaciaP fila col tablero)) nil)
		;;Verificacion filas
		((estaLista numero (get-Fila fila tablero)) nil)		  
		;; Verificacion columnas
		((estaLista numero (get-Columna col tablero)) nil)		  
		;; Verificacion cuadrantes
		((estaLista numero (get-Cuadrante fila col tablero)) nil)
		;; Salida si todo se cumple
		(t t)
	)      
)

;;; Comprobar que un numero es miembro de la lista, si existe en esa lista devolvera T, si no existe devolvera NIL
(defun estaLista(numero lista)
	(not (null (member numero lista)))
)

;;; Comprobar que la casilla esta vacia
(defun vaciaP (fila col tablero)
  (zerop (nth col (nth fila tablero)))
)

;;; Devolver la fila
(defun get-Fila (fila tablero)
  (nth fila tablero)
)

;;; Comprobar que el numero esta en la columna 
(defun get-Columna (col tablero)
	(list (nth col (first tablero)) (nth col (second tablero)) (nth col (third tablero)) (nth col (fourth tablero)))
)

;;; Devolver el cuadrante de la posición
;;; Ej: si fila 1 => que no esta en cuadrante 2,3 (puede estar o en 0 o en 1) y si col 1 ya sabemos que cuadrante es 0.
(defun get-Cuadrante (fila col tablero)
	; Compara el valor de la fila y el de la columna para saber a que cuadrante pertenece
	(case fila
			(0 (if (or (equal col 0) (equal col 1))  (setq nCuadrante 0) (setq nCuadrante 1) )) 
			(1 (if (or (equal col 0) (equal col 1))  (setq nCuadrante 0) (setq nCuadrante 1) )) 
			(2 (if (or (equal col 0) (equal col 1))  (setq nCuadrante 2) (setq nCuadrante 3) )) 
			(3 (if (or (equal col 0) (equal col 1))  (setq nCuadrante 2) (setq nCuadrante 3) )) 
	)	
	; Coge el numero de cuadrante y genera la lista de ese cuadrante
	(cond 
		  ((equal nCuadrante 0) (list  (first (first tablero)) (second (first tablero)) (first (second tablero)) (second (second tablero))))
		  ((equal nCuadrante 1) (list  (third (first tablero)) (fourth (first tablero)) (third (second tablero)) (fourth (second tablero))))
		  ((equal nCuadrante 2) (list  (first (third tablero)) (second (third tablero)) (first (fourth tablero)) (second (fourth tablero))))
		  ((equal nCuadrante 3) (list  (third (third tablero)) (fourth (third tablero)) (third (fourth tablero)) (fourth (fourth tablero))))			
	)
)

 
;; Funciones para la aplicacion de busqueda en espacio de estados
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Obtener todos los posibles estados descendientes del estado que se pasa por argumento 
(defun siguiente (estado)
  (setq lista ())
  (dotimes (n 4)
    (if (make-ESTADO-rellenar (+ n 1) estado)
	  (setq lista (append lista (list (make-ESTADO-rellenar (+ n 1) estado))))
    )
  )
  lista
)

;;; Comprobar si el estado es meta
(defun meta (estado)
  (if (equal (ESTADO-descubiertos estado) 16) t)
)


;;; Encontrar la solucion mediante busqueda en profundidad
(defun sol-sudoku-prof ()
	(format t "Introduzca un numero de casillas solucionadas:  ") 
	(setq nUbicar (read))
	(loop 
		(if (<= nUbicar 16) (return))
		(format t "Introduzca un numero menor a 16:  ") 
		(setq nUbicar (read))
	)	

	(setq solucion (busqProfundidad  (list (colocarNumeros nUbicar)) #'meta #'siguiente))
	(format t "TABLERO SOLUCION (busqueda en profundidad)~%")
	(if solucion 
		(imprime solucion)
		(format t "No existe solucion~%")
	)
)

;;; Encontrar la solucion mediante busqueda en anchura
(defun sol-sudoku-anch ()
	(format t "Introduzca un numero de casillas solucionadas:  ") 
	(setq nUbicar (read))
	(loop 
		(if (<= nUbicar 16) (return))
		(format t "Introduzca un numero menor a 16:  ") 
		(setq nUbicar (read))
	)
	(setq solucion (busqAnchura  (list (colocarNumeros nUbicar)) #'meta #'siguiente))
	(format t "TABLERO SOLUCION (busqueda en anchura)~%") 
	(if solucion 
		(imprime solucion)
		(format t "No existe solucion~%")
	)
)

;;; Algoritmo de busqueda en profundidad
(defun busqProfundidad (nodes meta siguiente)
  (cond ((null nodes) nil)
        ;; Devuelve el primer nodo si es final
        ((funcall meta (first nodes)) (first nodes))
        ;; Poner los hijos al frente de la lista
        (t (busqProfundidad (append (funcall siguiente (first nodes))  (rest nodes))
                meta
                siguiente)))
)

;;; Algoritmo de busqueda en anchura
(defun busqAnchura (nodes meta siguiente)
  (cond ((null nodes) nil)
        ;; Devuelve el primer nodo si es final
        ((funcall meta (first nodes)) (first nodes))
        ;; Poner los hijos al final de la lista
        (t (busqAnchura (append (rest nodes) (funcall siguiente (first nodes)))
                meta
                siguiente)))
)
