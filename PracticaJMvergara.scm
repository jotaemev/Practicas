(define (mrember l argl)
  (cond
   ;; Caso de finalizacion (se pone primero)
   ((null? l) '())
   ;; Caso especial: que hacemos si se da el caso particular?
   ((eq? (car l) argl) (mrember (cdr l) argl))
   ;; Caso general: que hacemos en todos los demas casos?
   (else
    (cons (car l) (mrember (cdr l) argl)))))

(display
 (mrember '(a b c a c a a f) 'a))
(newline)

(define (insert-right l arg1 arg2)
  (cond
   ((null? l) '())
   ((eq? (car l) arg1) 
    (cons (car l) (cons arg2 (insert-right (cdr l) arg1 arg2))))
   (else
    (cons (car l) (insert-right (cdr l) arg1 arg2)))))

(display
 (insert-right '(a b c d e f g h) 'c 'R))
(newline)

(define (insert-left l arg1 arg2)
  (cond
   ((null? l) '())
   ((eq? (car l) arg1)
    (cons arg2 (cons (car l) (insert-left (cdr l) arg1 arg2))))
   (else
    (cons (car l) (insert-left (cdr l) arg1 arg2)))))

(display
 (insert-left '(a b c d e f g h) 'e 'L))
(newline)

(define (msust l arg1 arg2)
  (cond
   ((null? l) '())
   ((eq? (car l) arg1)
    (cons arg2 (msust (cdr l) arg1 arg2)))
   (else
    (cons (car l) (msust (cdr l) arg1 arg2)))))

(display
 (msust '(a b a b a b a) 'a 'S))
(newline)

(define (element-append l new)
  (cond
   ((null? l) (cons new '()))
   (else
    (cons (car l) (element-append (cdr l) new)))))

(display
 (element-append '(a b c d e) 'f))
(newline)


(define (sust l old new)
  (cond
   ((null? l) '())
   ((eq? (car l) old)
    (cons new (cdr l)))
   (else
    (cons (car l) (sust (cdr l) old new)))))


(display
 (sust '(d c b a b a b a b) 'a 'S))
(newline)
 
(define (rember l sup)
  (cond
   ((null? l) '())
   ((eq? (car l) sup) (cdr l))
   (else
    (cons (car l) (rember (cdr l) sup)))))
      
(display
 (rember '(a b c a b c a b c) 'c))
(newline)

(define (nth-element l n)
  (cond
   ((null? l) '())
   ((= n 0) 
    (car l))
   (else
    (nth-element (cdr l) (- n 1)))))

(display
 (nth-element '(a b c d e f g) 3))
(newline)

(define (nth-rember l n)
  (cond
   ((null? l) '())
   ((= n 0)
    (cdr l))
   (else
    (cons (car l) (nth-rember (cdr l) (- n 1))))))
(display
 (nth-rember '(a b c d e f g) 3))
(newline)

(define (element-occur l n)
  (cond
   ((null? l) 0)
  ((eq? (car l) n)
   (+ 1 (element-occur (cdr l) n)))
  (else
   (+ 0 (element-occur (cdr l) n)))))
 

(display
(element-occur '(a b a b c a a d a) 'a))
(newline)


;;EJERCICIOS CON LET RECUR


(define (lrember l arg1)
  (let recur ((list l))
    (cond
     ((null? list) '())
     ((eq? (car list) arg1) (cdr list))
     (else
      (cons (car list) (recur (cdr list)))))))   

(display
 (lrember '(a b c d e) 'c))
(newline)

(define (lmrember l argl)
  (let recur ((list l))
  (cond
   ;; Caso de finalizacion (se pone primero)
   ((null? list) '())
   ;; Caso especial: que hacemos si se da el caso particular?
   ((eq? (car list) argl) (recur (cdr list)))
   ;; Caso general: que hacemos en todos los demas casos?
   (else
    (cons (car list) (recur (cdr list)))))))

(display
 (lmrember '(a b c a c a a f) 'a))
(newline)

(define (linsert-right l arg1 arg2)
  (let recur ((list l))
  (cond
   ((null? list) '())
   ((eq? (car list) arg1) 
    (cons (car list) (cons arg2 (recur (cdr list)))))
   (else
    (cons (car list) (recur (cdr list)))))))

(display
 (linsert-right '(a b c d e f g h) 'c 'R))
(newline)

(define (linsert-left l arg1 arg2)
  (let recur ((list l))
  (cond
   ((null? list) '())
   ((eq? (car list) arg1)
    (cons arg2 (cons (car list) (recur (cdr list)))))
   (else
    (cons (car list) (recur (cdr list)))))))

(display
 (linsert-left '(a b c d e f g h) 'c 'R))
(newline)

(define (lmsust l arg1 arg2)
  (let recur ((list l))
  (cond
   ((null? list) '())
   ((eq? (car list) arg1)
    (cons arg2 (recur (cdr list))))
   (else
    (cons (car list) (recur (cdr list)))))))

(display
 (lmsust '(a b a b a b a) 'a 'S))
(newline)

(define (lelement-append l new)
  (let recur ((list l))
  (cond
   ((null? list) (cons new '()))
   (else
    (cons (car list) (recur (cdr list)))))))

(display
 (lelement-append '(a b c d e) 'f))
(newline)

(define (lsust l old new)
  (let recur ((list l))
  (cond
   ((null? list) '())
   ((eq? (car list) old)
    (cons new (cdr list)))
   (else
    (cons (car list) (recur (cdr list)))))))


(display
 (lsust '(d c b a b a b a b) 'a 'S))
(newline)

(define (lnth-element l n)
  (let recur ((list l)
              (counter n))
    (cond
     ((null? list) '())
     ((= counter 0) 
      (car list))
     (else
      (recur (cdr list) (- counter 1))))))

(display
 (lnth-element '(a b c d e f g) 3))
(newline)

