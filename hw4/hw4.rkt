#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; these definitions are simply for the purpose of being able to run the tests
;; you MUST replace them with your solutions
;;

(define (sequence low high stride)
         (if (> low high)
             '()
             (cons low (sequence (+ low stride) high stride))
              ))

(define (string-append-map xs suffix)
        (map (lambda (x) (string-append x suffix)) xs))


(define (list-nth-mod xs n)
        (cond
          [(< n 0)(error "list-nth-mod: negative number")]
          [(null? xs)(error "list-nth-mod: empty list")]
          [#t(let ([rem (remainder n (length xs))]) (car (list-tail xs rem)))]))

(define (stream-for-n-steps s n)
        (if (= n 0)
            null
            (let ([next (s)])
                 (cons (car next) (stream-for-n-steps (cdr next) (- n 1))))))

(define funny-number-stream
        (letrec ([f (lambda (x)
                (cons (if (equal? (remainder x 5) 0) (- 0 x) x) (lambda () (f (+ x 1)))))])
          (lambda () (f 1))))

(define cat-then-dog 
        (lambda () (cons "cat.jpg"
                   (lambda () (cons "dog.jpg"
                                    cat-then-dog)))))

(define (stream-add-zero s)
        (let ([next (s)])
           (lambda () (cons (cons 0 (car next))
                     (stream-add-zero (cdr next))))))


(define (cycle-lists xs ys)
        (letrec ([f (lambda (n)
                (cons
                 (cons (list-nth-mod xs n) (list-nth-mod ys n))
                 (lambda () (f (+ n 1)))))])
          (lambda () (f 0))))

(define (vector-assoc v vec)
        (letrec ([f (lambda (n)
                (cond [(equal? n (vector-length vec)) #f]
                      [(pair? (vector-ref vec n)) (if (equal? (car (vector-ref vec n)) v) (vector-ref vec n) (f (+ n 1)))]
                      [#t (f (+ n 1))]))]) (f 0)))

(define cached-assoc null)


