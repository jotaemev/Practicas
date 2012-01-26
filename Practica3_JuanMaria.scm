(define (+tuple list1 list2)
  (cons (+ (car list1) (car list2)) (cons (+ (cadr list1) (cadr list2)) '())))

(display
 (+tuple '(1 2) '(5 4)))
 (newline)

;; (define (sum l)
;;   (let recur ((list l))
;;     (if (null? list)
;;         0
;;         (+ (car list) (recur (cdr list))))))

;; (display
;;  (sum '(1 2 3 4 5 6 7)))
;;  (newline)
    
(define (length l n)
  (let recur ((list l)
              (counter n))
    (cond
     ((null? list) counter)
     (else
      (recur (cdr list) (+ counter 1))))))

(display
 (length '(a b c d e) 0))
(newline)

(define (tuple-number? list)
  (cond
   ((null? list) #t)
   ((number? (car list)) (tuple-number? (cdr list)))
   (else
    #f)))

(display
 (tuple-number? '(1 2)))
(newline)

(define (list-number? list)
  (let recur ((l list))
    (cond
     ((null? l) #t)
     ((number? (car l)) (recur (cdr l)))
     (else
      #f))))

(display
 (list-number? '(1 2 1 4 5 6 1)))
(newline)

(define (show data)
  (display data)
  (newline))

(define (sum l)
  (if (list-number? l)
      (let recur ((list l))
        (if (null? list)
            0
            (+ (car list) (recur (cdr list)))))
      (error "Not a number list")))
     
(show (sum '(1 2 3 4 6 6)))

(define (filter list funcion)
  (let recur ((l list))
    (cond
     ((null? l) '())
     ((funcion (car l)) (cons (car l) (recur (cdr l))))
     (else
      (recur (cdr l))))))

(show (filter '(a 7 () d 6 5 f) symbol?))

(define (remove list funcion)
  (let recur ((l list))
    (cond
     ((null? l) '())
     ((funcion (car l)) (recur (cdr l)))
     (else
      (cons (car l) (recur (cdr l)))))))

(show (remove '(a 7 () d 6 5 f) number?))

(define (remove2 l f)
  (filter l (lambda (x) (not (f x)))))

(show (remove2 '(a 7 () d 6 5 f) number?))


(define (atom? e)
  (and (not (pair? e))
       (not (null? e))))

(define (map* f l)
  (let recur ((list l))
    (cond
     ((null? list) '())
     ((atom? (car list))
      (cons (f (car list)) (recur (cdr list))))
      (else
       (cons (recur (car list))
             (recur (cdr list)))))))

(show (map* (lambda (x) (* x x)) '(0 1 2 3 4 5)))

(define (insert-right* l e1 e2)
  (let recur ((list l))
    (cond
     ((null? list) '())
     ((atom? (car list))
      (if (eq? (car list) e1)
          (cons (car list) (cons e2 (recur (cdr list))))
          (cons (car list) (recur (cdr list)))))
     (else
      (cons (recur (car list))
            (recur (cdr list)))))))

(show (insert-right* '((a b) c d a (a)) 'a 'x))

(define (insert-left* l e1 e2)
  (let recur ((list l))
    (cond
     ((null? list) '())
     ((atom? (car list))
      (if (eq? (car list) e1)
          (cons e2 (cons (car list) (recur (cdr list))))
          (cons (car list) (recur (cdr list)))))
     (else
      (cons (recur (car list))
            (recur (cdr list)))))))

(show (insert-left* '((a b) c d a (a)) 'a 'x))