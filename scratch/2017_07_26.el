;; http://context.umcs.maine.edu/COS470/Assignments/lisp-practice.pdf

(defun first2 (list)
  (if (< (length list) 2)
      nil
    (cons (car list) (cons (nth-value 1 list) nil))))

;; first2 examples
(first2 '(1))
(first2 '(1 2 4 8 16))
(first2 '("cat" "mouse" "rabbit" "horse"))



(defun add1 (int)
  (1+ int))

;; add1 examples
(add1 4)
(add1 -8)
(add1 0)



(defun listadd1 (list)
  (if list
      (cons (1+ (car list)) (listadd1 (cdr list)))
    nil))

;; listadd1 examples
(listadd1 '(1 2 3 4 5))
(listadd1 '(4))
(listadd1 '())



(defun listadd2 (list)
  (mapcar '1+ list))

;; listadd1 examples
(listadd2 '(1 2 3 4 5))
(listadd2 '(4))
(listadd2 '())



;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Iteration.html#Iteration
;; (nconc '(1 2 3) '(4))
;; (nconc nil '(1))
(defun listadd3 (list)
  (let (value)
    (dolist (x list value)
      (print x)
      (print value)
      (setq value (nconc value (cons x ()))))))

;; listadd1 examples
(listadd3 '(1 2 3 4 5))
(listadd3 '(4))
(listadd3 '())



(defun flatten (list)             
  (if (listp list)                ;; if the list is actually a list
      (if list                    ;; if the list is not nil
	  (nconc                  ;; concatonate the flattened car and cdr
	   (flatten (car list))
	   (flatten (cdr list)))
	nil)                      ;; if the list is nil return nil
    (list list)))                 ;; if the list is a singleton make a list of it

;; flatten examples
(flatten '(a (b c)))
(flatten (car '(a (b c))))
(flatten '(a (b c) d (e (f (g (h) i))) j))
(flatten '(((z y a)) (b c) d (e (f (g (h) i))) j))
(flatten '())
(flatten '((((())))))
(flatten '(((a)) ((())) (b (c (d ()) e)) f))



(defun last2 (list)
  (setq len (length list))
  (if (< len  2)
      nil
    (nconc
     (list (nth-value (- len 2) list))
     (list (nth-value (- len 1) list)))))

;; last2 examples
(last2 '(1))
(last2 '(1 2 4 8 16))
(last2 '("cat" "mouse" "rabbit" "horse"))



(defun printlist ()
  ;; elisp 'format' command is different than common lisp
  nil)



(defun copyfile ()
  ;; skipping as this is probably very differnt in elisp
  nil)



;; Association Lists
(defvar *products* nil)

;; You will have to be careful, or sort will mangle your list of
;; products. Always either assign the value returned by sort back to
;; your original list’s location, or else copy the list (using
;; copy-tree, in this case) first, and pass the copy to sort:
;; ;; either do this:
;; (setq *products* (sort *products* mypred))
;; ;; or:
;; (setq other-location (sort (copy-tree *products* mypred)))

(defun add-product (list item qty)
  (let* ((found (assoc item list)) (value (cdr found)) (ptr (car list)))
    (if found                                            ;; if item is in list
	(setf (cdr found) (+ value qty))                 ;; increase the value
      (progn
	(setq new (cons item qty))
	(setf (car list) new)                            ;; modify list in place
	(setf (cdr list) ptr)))
    list))
;;      (cons (cons item qty) list))))                     ;; sketchy change

(defun delete-product (list item qty)
  (let* ((found (assoc item list)) (value (cdr found)))
    (if found                                            ;; if item is in list
	(if (>= value qty)
	    (setf (cdr found) (- value qty))             ;; decrease value
	  (assq-delete-all item list)))                  ;; don't use assq-delete-all
    list))




(car '(a . 1))
(cdr '(a . 1))

(car '(a 1))
(cdr '(a 1))

(setq grades '((Joe . A)
	      (Sally . B)
	      (Mary . A)
	      (Fred . B)))

(aref grades 0)
(elt grades 0)
(nth 0 grades)
(cdr (assoc 'Joe grades))
(assoc 'Sally grades)     ;; uses equal which allows different objects to be considered equal
(assq 'Sally grades)      ;; uses eq which typically requires objects to be one in the same
(nth 1 (assq 'Sally grades))
(aref 1 '(A . B))
(setq x (assoc 'Mary grades))
(setf (cdr x) 6)

grades

(setq *products* '((TV . 1) (computer . 2) (banana . 8)))
(add-product *products* 'computer 3)
*products*
(setq *products* (cons '(phone . 1) *products*))

(setq ret (add-product *products* 'crackers 50))
ret
*products*
(delete-product *products* 'banana 3)
