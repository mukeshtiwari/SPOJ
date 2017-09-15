#lang racket

;; converts digit to list of digits
;; try to add contract such that digit is integer
(define (digit-to-list digit)
  (map (λ (x) (- (char->integer x) 48)) (string->list (number->string digit))))

;;converts list of digit to digit
(define (list-to-digit digilist)
  (string->number
   (list->string
    (map (λ(x) (integer->char (+ x 48))) digilist))))
    

;; if every element in list is 9
(define (everything-is-nine nlist)
  (andmap (λ(x) (eq? x 9)) nlist))


(define (odd-list-palin fhalf  num)
  (let ([t (list-to-digit (append fhalf (rest (reverse fhalf))))])
    (cond
      [(> t num) t]
      [else (odd-list-palin (digit-to-list (+ 1 (list-to-digit fhalf))) num)])))

(define (even-list-palin fhalf num)
  (let ([t (list-to-digit (append fhalf (reverse fhalf)))])
    (cond
      [(> t num) t]
      [else (even-list-palin (digit-to-list (+ 1 (list-to-digit fhalf))) num)])))

(define (all-nine-palin lst)
  (let ([k (length lst)])
    (list-to-digit (append (list 1) (build-list (- k 1) (const '0)) (list 1)))))


(define (solve-for-n n)
  (cond 
    [(<= n 8) (+ n 1)]
    [else 
     (let* ([lst (digit-to-list n)]
            [len (length lst)])
       (cond
         [(everything-is-nine lst) (all-nine-palin lst)]
         [(even? len) (even-list-palin (take lst (quotient len 2)) n)]
         [(odd? len) (odd-list-palin (take lst (+ 1 (quotient len 2))) n)]))]))


(let loop ()
  (define a (read))
  (let/ec break
    (define (repeat cnt)
      (cond
        [(>= cnt a) (break)]
        [else
         (define b (read))
         (writeln (solve-for-n b))
         (repeat (+ 1 cnt))]))
    (repeat 0)))


