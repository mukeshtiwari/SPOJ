#lang racket

;; new solution using strings
;;split the string in the middle

(define (reverse-string/really-fast s)
  (define len (string-length s))
  (for ([c (in-string s)]
        [i (in-range (quotient len 2))])
    (define ni (- len i 1))
    (define nc (string-ref s ni))
    (string-set! s ni c)
    (string-set! s i nc))
  s)

(define (reverse-string/fast s)
  (reverse-string/really-fast (string-copy s)))


(define (split-string str)
  (let-values ([(quot rem) (quotient/remainder (string-length str) 2)])
    (substring str 0 (+ quot rem))))

(define (increment-string-by-one str)
  (number->string (+ (string->number str) 1)))

(define (all-nine-string str)
  (equal? (make-string (string-length str) #\9) str))

(define (merge-string-by-itself str bit)
  (let ([ln (string-length str)])
    (cond
      [(or (even? bit) (eq? ln 1)) (string-append str (reverse-string/fast str))]
      [else (string-append str (reverse-string/fast (substring str 0 (- ln 1))))])))



(define (search-for-number str n bit)
  (let ([ret-str (merge-string-by-itself str bit)])
    (cond
      [(> (string->number ret-str) n) ret-str]
      [else (search-for-number (increment-string-by-one str) n bit)])))

(define (solve-for-one-digit n)
  (cond
    [(<= n 8) (number->string (+ n 1))]
    [else (number->string (+ n 2))]))

(define (solve-for-multidigit str)
  (let* ([ln (string-length str)]
         [bit (if (even? ln) 0 1)]
         [tstr (split-string str)])
    (search-for-number tstr (string->number str) bit)))
        
  
(define (solve-for-str str)
  (cond
    [(eq? 1 (string-length str)) (solve-for-one-digit (string->number str))]
    [(all-nine-string str) (string-append "1" (make-string (- (string-length str) 1) #\0) "1")] 
    [else (solve-for-multidigit str)]))



(let loop ()
  (define a (string->number (read-line (current-input-port) 'any)))
  (let/ec break
    (define (repeat cnt)
      (cond
        [(>= cnt a) (break)]
        [else
         (define b (read-line (current-input-port) 'any))
         (fprintf (current-output-port) "~a\n"
                  (solve-for-str b))
         (repeat (+ 1 cnt))]))
    (repeat 0)))

