(define (+tuple list1 list2)
  (cons (+ (car list1) (car list2)) (cons (+ (cadr list1) (cadr list2)) '())))

(display
 (+tuple '(1 2) '(5 4)))
 (newline)

(define (sum l)
  (let recur ((list l))
    (if (null? list)
        0
        (+ (car list) (recur (cdr list))))))

(display
 (sum '(1 2 3 4 5 6 7)))
 (newline)
    
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
