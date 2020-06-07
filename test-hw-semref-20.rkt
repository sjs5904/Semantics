;#lang racket ;; uncomment this line
;(require "sol-hw6-20.rkt") ;; include your solution here
(require racket/trace)
(require "program.rkt")
(provide (all-defined-out)) 
(require racket/sandbox)

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (myequal1 EH1 EH2)
  (and (myequal (car EH1) (car EH2))
       (equal? (cadr EH1) (cadr EH2))))
       

(define (myequal l1 l2)
  (if (not (equal? (length l1) (length l2)))
      false
      (myeqlow l1 l2)))

(define (myeqlow l1 l2)
  (if (null? l1)
      true
      (and (present (car l1) l2)
          (myeqlow (cdr l1) l2))))

;; does some specific course present in the course list
(define (present x lst)
  (if (null? lst)
      false
      (if (equal? x (car lst))
          true
          (present x (cdr lst)))))



(define totalpoints 0) ;; total points for this assignment
(define cnt 0)  ;; test counts



(define (utest testcnt testname testfun testpoints)
  (begin
    (write testcnt)
    (write testname)
    (write ':)
    (write testpoints)
    (writeln 'pts)
    (with-handlers ([exn:fail? (lambda (exn) (begin (writeln "4sec Timeout"))) ])
    (with-deep-time-limit 4 
    (with-handlers ([exn:fail? (lambda (exn)
                                 (begin
                                   (writeln exn)
                                   (writeln "Exception")
                                   (writeln "incorrect")
                                   ;(set! totalpoints (- totalpoints testpoints))
                                   ))])
      (if (eval testfun ns)
          (begin
            (writeln "correct")
            (set! totalpoints (+ totalpoints testpoints)))
          (begin
            (writeln "incorrect output")
            ;(set! totalpoints (- totalpoints testpoints))
            ))
    )
    )
    )  
    ))


(define (hw6)
  (begin

    (writeln '************************************************)
    (writeln 'Tests-on-Q1)
    (writeln '************************************************)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-p01 '(myequal1 (sem p0 env1 hp0) '(((y 10) (x 1)) ((1 10) (2 free)))) 6)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-p01a '(myequal1 (sem p0 env1 hp01) '(((y 10) (x 2)) ((1 20) (2 10)))) 6)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-p02 '(myequal1 (sem p0 env1 hp02) '(((y 0) (x 0)) (oom))) 7)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-p1 '(myequal1 (sem p1 env1 hp1) '(((y 30) (x 3)) ((1 20) (2 20) (3 free)))) 6)
    

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-p2 '(myequal1 (sem p2 env1 hp2) '(((y 30) (x 4)) (ooma))) 7)


    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-p3a '(myequal1 (sem p3 env3a hp3) '(((y 10) (x 2) (z 1)) ((1 20) (2 10) (3 free)))) 6)
    

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-p3b '(myequal1 (sem p3 env3b hp3) '(((y 0) (x 2) (z 2)) (fma))) 7)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-p4 '(myequal1 (sem p4 env1 hp4) '(((x 2)) ((0 5) (1 1) (2 2) (3 free)))) 6)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-p5 '(myequal1 (sem p5 env1 hp5) '(((x 2)) (fma))) 7)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-p6a '(myequal1 (sem p6 env1 hp6a) '(((z 0) (y 6) (x 2)) ((1 10) (2 6) (3 4) (5 9) (6 2) (20 free)))) 7)


    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-p6b '(myequal1 (sem p6 env1 hp6b) '(((z 1) (y 20) (x 2)) ((1 10) (2 free) (3 4) (5 9) (6 20) (20 free) (21 free)))) 7)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-swap '(equal? (cadr (sem swap env1 hswap)) '((1 30) (2 10))) 6)
    
    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-part '(equal? (cadr (sem partition partenv heap1)) '((1 8) (2 4) (3 2) (4 9) (5 7) (6 10) (7 29) (8 35) (9 40) (10 20) (11 41))) 10)
    

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-sort '(equal? (cadr (sem qsort qenv heap1)) '((1 2) (2 4) (3 7) (4 8) (5 9) (6 10) (7 20) (8 29) (9 35) (10 40) (11 41))) 12)
    
    (writeln '---------------------------------------)
    (write "                      Total Points: ")
    (writeln totalpoints)

    
    )
)

(hw6)

