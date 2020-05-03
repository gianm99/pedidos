(defun prueba-pedido ()
    (inicializar-pedido 1)
    (setq prd1
        (make-producto :nombre "macarrones"
            :precio 4.5
            :id 4))
    (setq prd2
        (make-producto :nombre "ostras"
            :precio 1.5
            :id 2))
    (setq prd3
        (make-producto :nombre "fideos"
            :precio 7
            :id 3))
    (incluir-item prd1 3)
    (incluir-item prd2 5)
    (incluir-item prd3 1)
    (format t "~a" pedido)
)

(prueba-pedido)