; ------ TEMA ---------

(defun sum-list (numbers)
  (if (endp numbers) 
    0
    (if (symbolp(first numbers))
        (sum-list (rest numbers))
        (+ (first numbers) (sum-list (rest numbers))))
  )
)

(defun average-list (lista)
    (if (endp lista)
        nil
        (/ (sum-list lista) (list-length lista))
    )
)

(defun del-fa (element lista)
    ( if (endp lista)
        nil
        (if (equal (first lista ) element)
            (rest lista)
            (cons (first lista) (del-fa element (rest lista)))
        )    
    )
)

(defun par (l)
    (if (or (endp l) (= (length l) 1)) ;cazul in care lista are un singur element (folosim lungimea )
        '()
        (cons (second l) (par (cddr l))
        )
    )

)


;----------- LABORATOR ---------------


;;factorial

(defun factorial(n) 
    (if (<= n 0)
        1
        (* (factorial (- n 1)) n)
    )
)


;; fibonacci

(defun fib (n)
    (if (or (= n 0)(= n 1))
        1 ;;true
        (+ (fib(- n 1)) (fib(- n 2)))
    )
)

;; x la y (si puteri negative?)
(defun pow (x y)
    (if (= y 0)
        1
        (if (< y 0)
            (/ 1 (* x (pow x ( + y 1))))
            (* x (pow x (- y 1)))
    )
    )
)

;; reuniune multimi
(defun reuniune (list1 list2)
    (if (endp list1) ;;verificare daca lista este goala
        list2  ;;return list2(afiseaza)
        (reuniune (cdr list1) (adjoin (first list1) list2))
    )
)

;; intersectie multimi
(defun intersectie (list1 list2)
    (if (eq list1 ()) 
        list1
        (if (member (car list1) list2) ;;
            (cons (car list1) (intersectie (cdr list1) list2)) 
            (intersectie (cdr list1) list2)
        )
    )
)

(defun diferenta (list1 list2)
    (if list1
        (if (member (car list1) list2)
            (diferenta (cdr list1) list2)
            (cons (car list1) (diferenta (cdr list1) list2))
        )
    
    )
)


(defun diferenta-sim (l1 l2)
    (defun diff (list1 list2)
    (if list1
        (if (member (car list1) list2)
            (diferenta (cdr list1) list2)
            (cons (car list1) (diferenta (cdr list1) list2))
        )
    
    )
)
    (append (diff l1 l2) (diff l2 l1))
)

(defun first-atom ( lst )
    (cond ((endp lst) nil)
        (( atom (car lst)) (car lst))
        ((first-atom( car lst)))
    )
)

(defun inverte (lista)
  (inverte-aux lista () )
  )

(defun inverte-aux (lista restul)
  (if (null lista)
      restul
    (inverte-aux (rest lista) (cons (first lista) restul) )
    ))

(defun inv (l)
    (if (endp l)
        nil
        (concatenare (inv (cdr l)) (list (car l)))
    )
)

(defun concatenare (l1 l2)
    ( if (endp l1)
        l2
        (cons (car l1 ) (concatenare (cdr l1) l2))
    )
)













