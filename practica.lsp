;-----------------------------------------------------
; Simula la generación de pedidos de productos dentro 
; de una interfaz de usuario simple.
;
; Autores:  Gian Lucas Martín Chamorro
;           Irene Vera Barea
; Asignatura:   21721 - Lenguajes de Programación
;-----------------------------------------------------

;-----------------------------------------------------
; Inicia la ejecución del programa de creación de
; pedidos.
;-----------------------------------------------------
(defun inicio ()
    (fondo)
    (listar-productos) ; lista de los productos
)

;-----------------------------------------------------
; Dibuja las figuras del fondo del programa que se
; mantienen fijas.
;-----------------------------------------------------
(defun fondo ()
    (cls)
    (rectangulo-relleno 0 0 0 1 333 436 373) ; rectangulo del titulo
    (visualizar-palabra "productos" 84 342 2 10)
    (rectangulo 1 174 436 330) ; espacio de la lista de productos
    (rectangulo-relleno 0 0 0 1 136 637 170) ; letrero del pedido
    (rectangulo 1 33 637 133) ; espacio de productos del pedido
    (rectangulo 1 0 330 30) ; espacio del menu
    (rectangulo-relleno 0 0 0 331 0 637 30) ; espacio del total
    (visualizador "img\\logo.img" 440 174 200) ; logo del programa
)

;-----------------------------------------------------
; Lista los 20 productos del programa para hacer.
;-----------------------------------------------------
(defun listar-productos()
    ;; (escribir 3 1 "01. Cereales")
    ;; (escribir 3 22 "03.00")
    ;; (escribir 4 1 "03. Pan de molde")
    ;; (escribir 4 22 "01.00")
    ;; (escribir 5 1 "05. Gaseosa")
    ;; (escribir 5 22 "02.00")
    ;; (escribir 6 1 "07. Agua")
    ;; (escribir 6 22 "01.00")
    ;; (escribir 7 1 "09. Muffin")
    ;; (escribir 7 22 "03.00")
    ;; (escribir 8 1 "11. Huevos")
    ;; (escribir 8 22 "04.00")
    ;; (escribir 9 1 "13. Trufa")
    ;; (escribir 9 22 "14.00")
    ;; (escribir 10 1 "15. Harina")
    ;; (escribir 10 22 "01.00")
    ;; (escribir 11 1 "17. Cookie")
    ;; (escribir 11 22 "02.00")
    ;; (escribir 12 1 "19. Vino")
    ;; (escribir 12 22 "26.00")
    ;; (escribir 3 29 "02. Hielo")
    ;; (escribir 3 49 "03.00")
    ;; (escribir 4 29 "04. Cecina")
    ;; (escribir 4 49 "10.00")
    ;; (escribir 5 29 "06. Whisky")
    ;; (escribir 5 49 "30.00")
    ;; (escribir 6 29 "08. Pack de leche")
    ;; (escribir 6 49 "07.00")
    ;; (escribir 7 29 "10. Berberecho")
    ;; (escribir 7 49 "21.00")
    ;; (escribir 8 29 "12. Cigalas")
    ;; (escribir 8 49 "45.00")
    ;; (escribir 9 29 "14. Caviar")
    ;; (escribir 9 49 "75.00")
    ;; (escribir 10 29 "16. Langostinos")
    ;; (escribir 10 49 "45.00")
    ;; (escribir 11 29 "18. Calamar")
    ;; (escribir 11 49 "42.00")
    ;; (escribir 12 29 "20. Pizza")
    ;; (escribir 12 49 "05.00")
    ;; (goto-xy 0 22)
)

;-----------------------------------------------------
; Inicializa la lista de productos leyendo desde el 
; fichero pasado como argumento.
;-----------------------------------------------------
(defun inicializar-lista (nombre)
    (setq productos (make-array 20)) ; creación del array de 20 componentes
    (let ((fichero (open nombre :direction :input :if-does-not-exist nil)))
        (when fichero
            (dotimes (i 20)
                (setq nombre-producto (read-line fichero nil))
                (setq precio-producto (read-line fichero nil))
                (setf (aref productos i) (make-producto :nombre nombre-producto
                    :precio precio-producto
                    :id i))))))

;-----------------------------------------------------
; Visualiza el indicador del número de pedido dado en
; la pantalla usando imágenes. Solo funciona para n
; tal que 0 <= n < 100.
;-----------------------------------------------------
(defun indicador-pedido (n)
    (cond ((< n 10) (visualizar-palabra
            (concatenate 'string "pedido 0" (format nil "~A" n) 
            "                     ") 5 144 2 1 ))
        (t (visualizar-palabra
            (concatenate 'string "pedido " (format nil "~A" n) 
            "                     ") 5 144 2 1))))

;;-----------------------------------------------------------------------------
;; FUNCIONES BÁSICAS
;;-----------------------------------------------------------------------------

;-----------------------------------------------------
; Dibuja un rectángulo a partir de la coordenada 
; (x1,y1) hasta la (x2, y2).
;-----------------------------------------------------
(defun rectangulo (x1 y1 x2 y2)
	(move x1 y1)
	(draw x1 y2 x2 y2 x2 y1 x1 y1))

;-----------------------------------------------------
; Dibuja un rectángulo relleno de color (r,g,b) a
; partir de la coordenada (x1,y1) hasta la (x2, y2).
;-----------------------------------------------------
(defun rectangulo-relleno (r g b x1 y1 x2 y2)
	(color r g b)
	(rectangulo x1 y1 x2 y2)
	(dotimes (i (- y2 y1))
		(move x1 (+ i y1))
		(draw x2 (+ i y1))
	)
	(color 0 0 0)
)

;-----------------------------------------------------
; Dada una letra visualiza dicha letra utilizando la
; imagen del tipo dado
; tipo 1 --> ".img" (52 x 52 píxeles)
; tipo 2 --> "_NB.img" (20 x 20 píxeles)
;-----------------------------------------------------
(defun visualizar-letra (letra x y tipo)
	(if (equal letra " ") (setq letra "")) ; el caso de la letra espacio
    (cond ((= tipo 1)
            (visualizador (concatenate 'string "img/" letra ".img") x y 52))
		((= tipo 2)
            (visualizador (concatenate 'string "img/" letra "_NB.img") x y 20))))

;-----------------------------------------------------
; Dada una palabra la visualiza, letra a letra, a 
; partir de la coordenada (x,y) con imágenes del tipo 
; dado y con un espaciado dado entre ellas.
; tipo 1 --> ".img" (52 x 52 píxeles)
; tipo 2 --> "_NB.img" (20 x 20 píxeles)
;-----------------------------------------------------
(defun visualizar-palabra (palabra x y tipo espaciado)
    (dotimes (i (length palabra))
		(visualizar-letra (string (aref palabra i)) x y tipo)
		(color 0 0 0)
	    (if (= tipo 1) (setq x (+ 52 x espaciado)) (setq x (+ 20 x espaciado)))))

;-----------------------------------------------------
; Visualiza en pantalla la imagen dada por parámetro a
; partir de la coordenada (x,y) y de resolución 
; dimensión x dimension
;-----------------------------------------------------
(defun visualizador (imagen a b dimension)
    (setq fichero (open imagen :direction :input :element-type 'unsigned-byte))
    (setq x a y b)
    (dotimes (i dimension)
        (move x y)
        (dotimes (j dimension)
            ;leer valores de RGB
            (setq B (read-byte fichero nil))
            (setq G (read-byte fichero nil))
            (setq R (read-byte fichero nil))
            (if (null B) (return ()))
            (color R G B)
            (draw (+ 1 x) y)
            (setq x (+ 1 x))) ; incrementar x
        (setq x a y (+ 1 y))) ; reiniciar x e incrementar y
    (close fichero)
    (color 0 0 0))

;-----------------------------------------------------
; Borra a partir de la (linea,columna) dada el número 
; de columnas indicado de la pantalla en modo texto.
;-----------------------------------------------------
(defun borrar (linea columna numcolumnas)
	(goto-xy columna linea)
	(dotimes (i numcolumnas)
		(princ " ")
		(goto-xy (+ i columna) linea)
	)
)

;-----------------------------------------------------
; Visualiza el texto dado en la (linea,columna) dada 
; de la pantalla en modo texto.
;-----------------------------------------------------
(defun escribir (linea columna TEXTO)
	(goto-xy columna linea)
	(princ TEXTO)
)

;;-----------------------------------------------------------------------------
;; ESTRUCTURAS
;;-----------------------------------------------------------------------------

(defstruct producto
    nombre
    precio
    id
)