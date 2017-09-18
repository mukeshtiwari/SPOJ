#lang racket
(require math/number-theory)

(define (solve-for-number n)
  (let ([k (quotient n 2)])
    (cond
      [(odd? n) k]
      [else
       (if (= 1 (gcd n (- k 1))) (- k 1) (- k 2))])))


(let loop ()
  (define a (string->number (read-line (current-input-port) 'any)))
  (let/ec break
    (define (repeat cnt)
      (cond
        [(>= cnt a) (break)]
        [else
         (define b (read-line (current-input-port) 'any))
         (fprintf (current-output-port) "~a\n"
                  (solve-for-number (string->number b)))
         (repeat (+ 1 cnt))]))
    (repeat 0)))