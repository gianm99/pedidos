;-----------------------------------------------------
; Programa que simula la generación de pedidos de 
; productos dentro de una interfaz de usuario simple.
;
; Autores:  Gian Lucas Martín Chamorro
;           Irene Vera Barea
;-----------------------------------------------------

;-----------------------------------------------------
; Función que inicia la ejecución del programa de 
; creación de pedidos.
;-----------------------------------------------------
(defun inicio ()
)

;-----------------------------------------------------
; Función que dada una letra visualiza dicha letra
; utilizando la imagen del tipo dado
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
; Función que dada una palabra la visualiza,
; letra a letra, a partir de la coordenada (x,y) con 
; imágenes del tipo dado y con un espaciado dado entre
; ellas.
; tipo 1 --> ".img" (52 x 52 píxeles)
; tipo 2 --> "_NB.img" (20 x 20 píxeles)
;-----------------------------------------------------
(defun visualizar-palabra (palabra x y tipo espaciado)
    (cls)
    (dotimes (i (length palabra))
		(VisualizarLetra (string (aref palabra i)) x y tipo)
		(color 0 0 0)
		(cond ((= tipo 1) (setq x (+ 52 x espaciado)))
			(t (setq x (+ 20 x espaciado))))))

;-----------------------------------------------------
; Función que visualiza en pantalla la imagen dada por
; parámetro a partir de la coordenada (x,y) y de
; resolución dimensión x dimension
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
; Función que dibuja un rectángulo a partir de la 
; coordenada (x1,y1) hasta la (x2, y2).
;-----------------------------------------------------
(defun rectangulo (x1 y1 x2 y2)
	(move x1 y1)
	(draw x1 y2 x2 y2 x2 y1 x1 y1)
)

;-----------------------------------------------------
; Función que dibuja un rectángulo relleno de color 
; (r,g,b) a partir de la coordenada (x1,y1) hasta la
; (x2, y2).
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
; Función que dibuja las figuras del fondo del 
; programa que se mantienen fijas.
;-----------------------------------------------------
(defun fondo ()
    (cls)
    (rectangulo-relleno 0 0 0 1 333 436 373)
    (rectangulo 1 174 436 330)
    (rectangulo-relleno 0 0 0 1 136 637 170)
    (rectangulo 1 33 637 133)
    (rectangulo 1 0 330 30)
    (rectangulo-relleno 0 0 0 331 0 637 30)
    ;; (visualizador "pedidos-lisp\\img\\LogoPractica.img" 440 174 200)
    )
