;-----------------------------------------------------
; Simula la generación de pedidos de productos dentro
; de una interfaz de usuario simple.
;
; Autores:      Gian Lucas Martín Chamorro
;               Irene Vera Barea
; Asignatura:   21721 - Lenguajes de Programación
;-----------------------------------------------------

;;***************************************************************************;;
;;                              ESTRUCTURAS                                  ;;
;;***************************************************************************;;

;-----------------------------------------------------
; Representa un producto en concreto.
;-----------------------------------------------------
(defstruct producto
    nombre
    precio
    id  ; Id único del producto
)

;-----------------------------------------------------
; Representa un componente de un pedido. Sirve para 
; relacionar los productos con las cantidades.
;-----------------------------------------------------
(defstruct item
    producto
    cantidad
    subtotal  ; precio * cantidad
)

;-----------------------------------------------------
; Representa un pedido de productos.
;-----------------------------------------------------
(defstruct pedido
    numero  ; número de pedido
    total  ; coste total
    items  ; lista de ítems
)

;;***************************************************************************;;
;;                              FUNCIONES                                    ;;
;;***************************************************************************;;

;-----------------------------------------------------
; Inicia la ejecución del programa de creación de
; pedidos.
;-----------------------------------------------------
(defun inicio ()
    (fondo)
    (inicializar-lista "productos.txt")
    (listar-productos)
    (logo)
    (menu))

;------------------------------------------------------------------------------
;                                   MENÚ
;------------------------------------------------------------------------------

;-----------------------------------------------------
; Inicia un pedido, pide el número de pedido,
; muestra el número de pedido, permite seleccionar
; producto, las unidades de este y el total del pedido
; Finalmente, guarda todo en un archivo e informa
; al usuario.
;-----------------------------------------------------
(defun menu()
    (iniciar-pedido)
    (anadir-productos))

;-----------------------------------------------------
; Pide al usuario si quiere iniciar el pedido
;-----------------------------------------------------
(defun iniciar-pedido()
    (limpiar-menu)
    (princ "[] Iniciar pedido (S/N): ")
    (setq a (read))
    (cond ((string-equal a "S") (pedir-numero))
        (t (menu))
    )
    (setq contadorItem 0)
    (mostrar-total (pedido-total pedido))
    (goto-xy 1 23))

;-----------------------------------------------------
; Pide el número de pedido, lo inicializa y lo
; imprime en pantalla.
;-----------------------------------------------------
(defun pedir-numero()
    (limpiar-menu)
    (princ "[] Numero de pedido: ")
    (setq num (read))
    (inicializar-pedido num)  ; crea el pedido
    (indicador-pedido num))  ; imprime la información

;-----------------------------------------------------
; Pide el número del producto que desea añadir,
; las cantidades que quiere de este y confirma
; si lo que pide es correcto para añadirlo al pedido.
;-----------------------------------------------------
(defun anadir-productos()
    (limpiar-menu)
    (princ "[] Numero de producto: ")
    (setq num (read))
    (if (or (< num 1) (> num 20))
        ((limpiar-menu)
        (princ "[] No existe. Insertar otro (S/N): ")
        (setq insertarnuevo (read))
        (cond ((string-equal insertarnuevo "S") (anadir-productos))
            (t (continuar-pedido)))))
    (imagen-producto num)
    (limpiar-menu)
    (format t "[] Unidades de ~a: "
        (producto-nombre (aref productos (- num 1))))
    (setq cant (read))
    (limpiar-menu)
    (format t "[] ~d de ~a (S/N): "
        cant
        (producto-nombre (aref productos (- num 1))))
    (setq confirm (read))
    (logo)
    (cond ((string-equal confirm "S")
            (incluir-item (aref productos (- num 1)) cant)
            (if (= (mod contadorItem 18) 0) 
                (rectangulo-relleno 255 255 255 2 34 636 135))
            (setq contadorItem (+ 1 contadorItem))
            (color 0 0 0)
            (imprimir-item contadorItem)
            (mostrar-total (pedido-total pedido))))
    (continuar-pedido))

;-----------------------------------------------------
; Pide al usuario si quiere continuar con el pedido.
; Si se quiere continuar se le pide que añada más
; productos, si no se guarda el pedido y se pregunta
; si quiere hacer otro.
;-----------------------------------------------------
(defun continuar-pedido()
    (limpiar-menu)
    (princ "[] Continuar pedido (S/N): ")
    (setq continuar(read))
    (cond ((string-equal continuar "S") (anadir-productos))
        (t (guardar-pedido)
            (fin-pedido))))

;-----------------------------------------------------
; Informa al usuario de que se ha guardado su pedido
; y le pregunta si quiere hacer otro.
;-----------------------------------------------------
(defun fin-pedido()
    (limpiar-menu)
    (princ "[] Guardado. Nuevo pedido (S/N): ")
    (setq nuevoPedido(read))
    (cond ((string-equal nuevoPedido "S") 
            (tapar-total)
            (menu))
        (t (exit))))

;-----------------------------------------------------
; Guarda el pedido actual en un fichero de texto. El
; fichero se llamará pedidoXX.txt, siendo XX el número
; del pedido. Si ya existe, se sobreescribirá.
;-----------------------------------------------------
(defun guardar-pedido ()
    (let ((fichero (open (format nil "pedidos/pedido~2,'0d.txt" 
            (pedido-numero pedido))
        :direction :output
        :if-exists :supersede  ; Sobreescribir si existe
        :if-does-not-exist :create)))  ; Crear si no existe
        (when fichero
            (format fichero "PEDIDO ~2,'0d~%" (pedido-numero pedido))
            (format fichero "~17@a~35@a~36@a~%"
                "PRODUCTOS"
                "UNIDADES"
                "IMPORTE")
            (dotimes (i  (length (pedido-items pedido)))
                (setf item (nth i (pedido-items pedido)))
                (format fichero "~16a~33d~37,2f euros~%"
                    (producto-nombre (item-producto item))
                    (item-cantidad item)
                    (item-subtotal item)))
            (format fichero "TOTAL PEDIDO~11,2f euros~%"
                (pedido-total pedido)))
        (close fichero)))

;------------------------------------------------------------------------------
;                            LISTA DE PRODUCTOS
;------------------------------------------------------------------------------

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
                (setq precio-producto (parse-float (read-line fichero nil)))
                (setf (aref productos i)
                    (make-producto
                        :nombre nombre-producto
                        :precio precio-producto
                        :id i))))
        (close fichero)))

;-----------------------------------------------------
; Imprime la lista de los productos con su información.
;-----------------------------------------------------
(defun listar-productos ()
    (setq i 0)
    (dotimes (i 10)
        (setq fila (+ 3 i))
        (escribir fila 1 (format nil "~2,'0d. ~16a ~5,2f" (+ 1 (* 2 i))
            (producto-nombre (aref productos (* 2 i)))
            (producto-precio (aref productos (* 2 i)))))
        (escribir fila 28 (format nil "~2,'0d. ~16a ~5,2f" (+ 2 (* 2 i))
            (producto-nombre (aref productos (+ 1 (* 2 i))))
            (producto-precio (aref productos (+ 1 (* 2 i))))))
        (setq i (+ 1 i))))

;------------------------------------------------------------------------------
;                         INFORMACIÓN DEL PEDIDO
;------------------------------------------------------------------------------

;-----------------------------------------------------
; Inicializa una instancia de la estructura pedido y
; le asigna numero como número de pedido.
;-----------------------------------------------------
(defun inicializar-pedido (numero)
    (setq pedido
        (make-pedido :numero numero
            :total 0
            :items '())))

;-----------------------------------------------------
; Inicializa una instancia de la estructura item y la
; añade al pedido actual. Se le asigna el producto y
; la cantidad pasados como argumentos.
;-----------------------------------------------------
(defun incluir-item (producto cantidad)
    (setq item
        (make-item :producto producto
            :cantidad cantidad
            :subtotal (* cantidad (producto-precio producto))))
    (setf (pedido-items pedido) (append (pedido-items pedido) (list item))
        (pedido-total pedido) (+ (pedido-total pedido) (item-subtotal item))))

;-----------------------------------------------------
; Imprime los ítems que se van añadiendo al pedido.
;-----------------------------------------------------
(defun imprimir-item(contadorItem)
    (setq offset (mod (- contadorItem 1) 18))
    (setf item (car (last (pedido-items pedido))))
    (setq linea (+ 16 (floor offset 3)))
    (setq columna (+ 1 (* 26 (mod offset 3))))
    (escribir linea columna (format nil "[~12a/~3,'0d/~7,2f]"
        (producto-nombre (item-producto item))
        (item-cantidad item)
        (item-subtotal item))))

;------------------------------------------------------------------------------
;                            CONVERSIÓN DE NÚMEROS
;------------------------------------------------------------------------------

;-----------------------------------------------------
; Convierte la representación en string de un float
; en un float y lo devuelve. Solo funciona con números
; positivos. La coma de los números decimales tiene
; que ser un punto (".").
;-----------------------------------------------------
(defun parse-float (s)
    (setq nf 0)
    (setq lf (length s))
    (setq pp (position #\. s))  ; Posición del punto
    (cond ((not pp) (setq nf (parse-integer s)))  ; No hay parte decimal
        (t (setq nf (parse-integer (subseq s 0 pp)))
            (dotimes (i (- lf pp 1))
                (setq nf (+ nf (* (- (char-code (char s (+ pp i 1))) 48)
                    (expt 10 (- (+ i 1)))))))))
    (float nf))  ; Devuelve el número en formato float

;-----------------------------------------------------
; Convierte la representación en string de un integer
; en un integer y lo devuelve. Solo funciona con
; números positivos.
;-----------------------------------------------------
(defun parse-integer (s)
    (setq ni 0)
    (setq li (length s))
    (dotimes (j li)
        (setq ni (+ ni (* (- (char-code (char s j)) 48)
            (expt 10 (- li j 1))))))
    ni)  ; Devuelve el número

;------------------------------------------------------------------------------
;                                  IMAGEN
;------------------------------------------------------------------------------

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
	    (if (= tipo 1)
            (setq x (+ 52 x espaciado))
            (setq x (+ 20 x espaciado)))))

;-----------------------------------------------------
; Dada una letra visualiza dicha letra utilizando la
; imagen del tipo dado
; tipo 1 --> ".img" (52 x 52 píxeles)
; tipo 2 --> "_NB.img" (20 x 20 píxeles)
;-----------------------------------------------------
(defun visualizar-letra (letra x y tipo)
	(if (equal letra " ") (setq letra "")) ; el caso de la letra espacio
    (cond ((= tipo 1)
            (visualizador (format nil "img/~a.img" letra) x y 52))
		((= tipo 2)
            (visualizador (format nil "img/~a_NB.img" letra) x y 20))))

;-----------------------------------------------------
; Visualiza en pantalla la imagen dada por parámetro a
; partir de la coordenada (x,y) y de resolución
; dimensión x dimension
;-----------------------------------------------------
(defun visualizador (imagen a b dimension)
    (setq fichero (open imagen
        :direction :input
        :element-type 'unsigned-byte))
    (setq x a y b)
    (dotimes (i dimension)
        (move x y)
        (dotimes (j dimension)
            ;leer valores de RGB
            (setq B (read-byte fichero nil)
                G (read-byte fichero nil)
                R (read-byte fichero nil))
            (if (null B) (return ()))
            (color R G B)
            (draw (+ 1 x) y)
            (setq x (+ 1 x))) ; incrementar x
        (setq x a y (+ 1 y))) ; reiniciar x e incrementar y
    (close fichero)
    (color 0 0 0))

;-----------------------------------------------------
; Visualiza la imagen que corresponda al número n. Si
; n es menor a 0 o mayor a 19 no hace nada.
;-----------------------------------------------------
(defun imagen-producto (n)
    (cond ((and (> n 0) (<= n 20))
        (visualizador (format nil "img/producto~a.img" n) 440 174 200))))

;-----------------------------------------------------
; Visualiza la imagen del logo del programa.
;-----------------------------------------------------
(defun logo ()
    (visualizador "img/logo.img" 440 174 200)) ; logo del programa

;------------------------------------------------------------------------------
;                                  DIBUJADO
;------------------------------------------------------------------------------
;-----------------------------------------------------
; Dibuja las figuras del fondo del programa que se
; mantienen fijas.
;-----------------------------------------------------
(defun fondo ()
    (cls)
    (rectangulo-relleno 0 0 0 1 333 436 373) ; rectangulo del titulo
    (visualizar-palabra "productos" 84 342 2 10)
    (rectangulo 1 174 436 330) ; espacio de la lista de productos
    (rectangulo-relleno 0 0 0 1 139 637 170) ; letrero del pedido
    (rectangulo 1 33 637 136) ; espacio de productos del pedido
    (rectangulo 1 0 330 33) ; espacio del menu
    (rectangulo-relleno 0 0 0 331 0 637 30)) ; espacio del total

;-----------------------------------------------------
; Visualiza el indicador del número de pedido dado en
; la pantalla usando imágenes. Solo funciona para n
; tal que 0 <= n < 100.
;-----------------------------------------------------
(defun indicador-pedido (n)
    (visualizar-palabra
        (format nil "~30a" (format nil "pedido ~2,'0d" n)) 5 144 2 1))
        
;-----------------------------------------------------
; Visualiza el coste total del pedido.
;-----------------------------------------------------
(defun mostrar-total(n)
    (visualizar-palabra (format nil "total") 334 6 2 1)
    (visualizar-palabra (format nil "~9,2,'0f" n) 444 6 2 1))

;-----------------------------------------------------
; Tapa el indicador de total del pedido.
;-----------------------------------------------------
(defun tapar-total ()
    (rectangulo-relleno 0 0 0 331 0 637 30))

;-----------------------------------------------------
; Limpia el menú y deja el cursor listo para escribir
; nueva información.
;-----------------------------------------------------
(defun limpiar-menu ()
    (borrar 23 2 40)  ; borrar lo escrito
    (goto-xy 1 23))  ; situar el cursor en la posicion inicial

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
		(draw x2 (+ i y1)))
	(color 0 0 0))

;------------------------------------------------------------------------------
;                                  TEXTO
;------------------------------------------------------------------------------

;-----------------------------------------------------
; Borra a partir de la (linea,columna) dada el número
; de columnas indicado de la pantalla en modo texto.
;-----------------------------------------------------
(defun borrar (linea columna numcolumnas)
	(goto-xy columna linea)
	(dotimes (i numcolumnas)
		(princ " ")
		(goto-xy (+ i columna) linea)))

;-----------------------------------------------------
; Visualiza el texto dado en la (linea,columna) dada
; de la pantalla en modo texto.
;-----------------------------------------------------
(defun escribir (linea columna TEXTO)
	(goto-xy columna linea)
	(format t TEXTO))
