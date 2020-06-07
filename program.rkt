#lang racket
(provide (all-defined-out))



;;;

(define env1 '())
(define hp0 '((1 free) (2 free)))
(define hp01 '((1 20) (2 free)))
(define hp02 '((1 20) (2 30)))
(define p0
  '(
    (decl x)
    (decl y)
    (ref x 10)
    (deref y x)
   )
)  

(define hp1 '((1 20) (2 20) (3 free)))
(define p1
  '(
    (decl x)
    (decl y)
    (ref x 10)
    (wref x 30)
    (deref y x)
    (free x)
   )
)

(define hp2 hp1)
(define p2
  '(
    (decl x)
    (decl y)
    (ref x 10)
    (wref x 30)
    (deref y x)
    (assign x (+ x 1))
    (free x)
   )
)

(define env3a '((z 1)))
(define env3b '((z 2)))
(define hp3 '((1 20) (2 free) (3 free)))
(define p3
  '(
    (decl x)
    (decl y)
    (ref x 10)
    (if (gt z 1)
        (
         (free x)
        ))
    (deref y x)
   )
)  

(define hp4 '((0 5) (1 free) (2 free) (3 free)))
(define p4
  '(
    (decl x)
    (ref x 1)
    (ref x 2)
    (deref x x)
    (deref x x)
   )
)  

(define hp5 hp4)
(define p5
  '(
    (decl x)
    (ref x 1)
    (ref x 2)
    (deref x x)
    (free x)
    (deref x x)
   )
)

(define hp6a '((1 10) (2 free) (3 4) (5 9) (6 free) (20 free)))
(define hp6b '((1 10) (2 free) (3 4) (5 9) (6 20) (20 free) (21 free)))
(define p6
  '(
    (decl x)
    (decl y)
    (decl z)
    (ref x 0)
    (ref y 0)
    (wref x y)
    (wref y x)
    (assign z 0)
    (if (gt (- y x) 5)
        (
         (free x)
         (free y)
         (assign z 1)
        ))
    
    
   )
)  

(define hswap '((1 10) (2 30)))
(define swap
  '(
     (fundecl (swap (x y)) (
                             (decl temp1)
                             (decl temp2)
                             (deref temp1 x)
                             (deref temp2 y)
                             (wref x temp2)
                             (wref y temp1)
                           )
     )
     (decl a)
     (decl b)
     (assign a 1)
     (assign b 2)
     (call (swap (a b)) 1)
   )
)


;(define heap1 '((1 10) (2 4) (3 40) (4 29) (5 7) (6 8) (7 9) (8 35)))

(define heap1 '((1 10) (2 4) (3 40) (4 29) (5 7) (6 8) (7 9) (8 35) (9 2) (10 20) (11 41)))
(define partenv '((pivot 1) (left 1) (right 11)))
(define partition
  '(
     (fundecl (swap (x y)) (
                             (decl temp1)
                             (decl temp2)
                             (deref temp1 x)
                             (deref temp2 y)
                             (wref x temp2)
                             (wref y temp1)
                           )
     )
     ;; partition
     (fundecl (partition (pivot left right))
              (  
                 (if (or (eq left right)
                         (gt left right))
                     (
                      (call (swap (pivot right)) 1)
                      (assign returnpart right)
                     )
                 )
                 (if (lt left right) ;; explicit else
                     (
                      (decl p)
                      (decl x)
                      (decl y)
                      (deref p pivot)
                      (deref x left)
                      (deref y right)
                      (if (or (eq x p) (lt x p)) ;; increment left index using recursion
                          (
                            (assign left (+ left 1))
                          )
                      )
                      (if (or (eq y p) (gt y p)) ;; decrement right index using recursion
                          (
                            (assign right (- right 1))
                          ) 
                      )
                      (deref x left) 
                      (deref y right)
                      (if (and (lt left right)
                               (and (gt x p)
                                    (lt y p))) ;; now we are ready to swap
                          (
                            (call (swap (left right)) 1)
                          )
                      )
                      (call (partition (pivot left right)) 1)
                     ) 
                 )
               )  
     )

     (decl returnpart)
     (call (partition (pivot left right)) 1) 
   )  
)

(define qenv '((fst 1) (lst 11)))
(define qsort
  '(
     (fundecl (swap (x y)) (
                             (decl temp1)
                             (decl temp2)
                             (deref temp1 x)
                             (deref temp2 y)
                             (wref x temp2)
                             (wref y temp1)
                           )
     )

     (fundecl (partition (pivot left right))
              (

                 (decl p)
                 (decl x)
                 (decl y)
                 (deref p pivot)
                 (deref x left)
                 (deref y right)
                 
                 (if (or (eq left right)    
                         (gt left right))  ;; left index is >= right index
                     (
                      (if (lt p x) (  
                                     (call (swap (pivot (- left 1))) 1)
                                     (assign returnpart (- left 1))
                                   )
                      )
                      (if (not (lt p x)) ( ;; we don't have else in the language
                                          (call (swap (pivot left)) 1)
                                          (assign returnpart left)
                                         )
                      )    
                     )
                 )
                    
                 (if (lt left right) ;; another explicit else
                     (
                      
                      (if (or (eq x p) (lt x p)) ;; increment left
                          (
                            (assign left (+ left 1))
                          )
                      )
                      (if (or (eq y p) (gt y p)) ;; decrement right 
                          (
                            (assign right (- right 1))
                          ) 
                      )
                      (deref x left) 
                      (deref y right)
                      (if (and (lt left right)
                               (and (gt x p)
                                    (lt y p))) ;; now we are ready to swap
                          (
                            (call (swap (left right)) 1)
                          )
                      )
                      (call (partition (pivot left right)) 1) ;; continue the iteration
                     ) 
                 )
               )  
     )

     
     (fundecl (qsort (left right))
              (
                (decl returnpart) ;; index returned by partitioning
                
                (if (eq left (- right 1)) ;; two elements base case
                    (
                      (decl x)
                      (decl y)
                      (deref x left)
                      (deref y right)
                      (if (gt x y)
                          (
                           (call (swap (left right)) 1)
                          ) 
                      ) 
                    )
                )
                   
                (if (lt left (- right 1)) ;; more than two elements
                    (
                     (call (partition (left left right)) 1) ;; will assign index to returnpart
                     ;; Now we need to qsort the elements to the left of returnpart
                     ;;                      the elements to the right of returnpart
                     (call (qsort (left (- returnpart 1))) 1)
                     (call (qsort ((+ returnpart 1) right)) 1)
                    )
                 )
                
              )
     )
     (call (qsort (fst lst)) 1)
   )  
)

