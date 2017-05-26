#lang racket
;; Programming Languages Homework4 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and change HOMEWORK_FILE to the name of your homework file.
(require "hw4.rkt")

(require rackunit)

;; Helper functions
(define ones (lambda () (cons 1 ones)))
(define aa 2)
(define b 0)

;(require test-engine/racket-tests)
;(define testserror
;  (test-suite "Name "
;               (check-error (list-nth-mod (list 0 1 2 3 4) -4)  "test error negative")
;))

(define-syntax-rule (handler s)
   (with-handlers ([(lambda (v) #t) (lambda (v) "this is an invalid value")]) s))

;; (handler s) wraps expression s so in case of run-time error
;; it simply returns #f

(define tests
  (test-suite
   "Sample tests for Assignment 4"

   ; sequence test
   (check-equal? (handler (sequence 0 5 1)) (list 0 1 2 3 4 5) "Sequence test")

   (check-equal? (handler (sequence 0 10 2)) (list 0 2 4 6 8 10) "Sequence test2")

   (check-equal? (handler (sequence 0 10 3)) (list 0 3 6 9) "Sequence test3")

   (check-equal? (handler (sequence 0 0 1)) (list 0) "sequence test 4")

   (check-equal? (handler (sequence 0 -1 1)) (list) "sequence test 5")

   (check-equal? (handler (sequence 0 -1 1)) (list) "sequence test 5")

 ; string-append-map test
   (check-equal?  (handler 
                  (string-append-map
                  (list "cat" "dog" "curry" "dog2")
                  ".jpg")) '("cat.jpg" "dog.jpg" "curry.jpg" "dog2.jpg") "string-append-map test")

   (check-equal? (handler
                  (string-append-map
                  (list "this" "is" "the" "sea") ".txt"))
                  (list "this.txt" "is.txt" "the.txt" "sea.txt") "string append 2")

   (check-equal? (handler
                  (string-append-map
                  (list ) ".txt")) (list) "string append empty list")

    ; list-nth-mod test
   (check-equal? (handler (list-nth-mod (list 0 1 2 3 4) 2)) 2 "list-nth-mod test")

   (check-equal? (handler (list-nth-mod (list 0 1 2 3 4) 10)) 0 "test rotating")


 ; stream-for-n-steps test
   (check-equal? (handler (stream-for-n-steps (lambda () (cons 1 ones)) 1)) (list 1) "stream-for-n-steps test")

    ; funny-number-stream test
   (check-equal? (handler (stream-for-n-steps funny-number-stream 16)) (list 1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16) "funny-number-stream test")

   (check-equal? (handler (stream-for-n-steps cat-then-dog 1)) (list "cat.jpg") "cat-then-dog test")

   (check-equal? (handler (stream-for-n-steps cat-then-dog 3)) (list "cat.jpg" "dog.jpg" "cat.jpg") "cat-then-dog test")

   (check-equal? (handler (stream-for-n-steps cat-then-dog 4)) (list "cat.jpg" "dog.jpg" "cat.jpg" "dog.jpg") "cat-then-dog test")

; cycle-lists test
   (check-equal? (handler
                    (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3))
                 (list (cons 1 "a") (cons 2 "b") (cons 3 "a"))
                 "cycle-lists test")

   (check-equal? (handler
                    (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a")) 4))
                 (list (cons 1 "a") (cons 2 "a") (cons 3 "a") (cons 1 "a"))
                 "cycle-lists test 2")

   (check-equal? (handler (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a")) 0))
                 (list)
                 "cycle-lists test 3")

   ; vector-assoc test
   (check-equal? (handler 
                   (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))))
                 (cons 4 1) "vector-assoc test")
   (check-equal? (handler 
                   (vector-assoc 1 (vector (cons 1 "1") "hi" (cons 2 "2") (cons 3 "3") (cons 4 "4"))))
                   (cons 1 "1") "vector-assoc test")
   (check-equal? (handler 
                   (vector-assoc -5 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))))
                    #f "vector-assoc test")










   

   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)