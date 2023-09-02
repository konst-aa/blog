# SPOILER
A short program to solve linear equations using gaussian elimination, along with a breakdown of how it works.  
# ENDSPOILER

## Linear Equation Solver 9000

### Flexing
Written during MATH16000 (linear algebra) using termux and vim, of course.  


### Breakdown
The idea is to take the first row, use that to cancel all *x1* in the rows below, then the second row to cancel all *x2*, and so on. Then you do the same thing, but backwards: use the last row to cancel all *xn* in the rows above, then the second to last row to cancell all *xn-1*, and so on. Last of all, divide each row by its *xn* to set it to 1.  

### Limitations
Currently, you need a non-zero *x1* in the first row, then a non-zero *x2* in the second row, and so on. Also this thing can't detect series of equations with infinite/no solutions.

### Code

```racket
#lang racket/main

;; Input
(list (list 1 -2 1 0)
      (list 0 2 -8 8)
      (list -4 5 9 -9))

;; Output
(list (list 1 0 0 29) 
      (list 0 1 0 16) 
      (list 0 0 1 3))

(define (scalar-mul s lst)
  (map (lambda (i) (* s i))
       lst))

(define (list-sub lst1 lst2)
  (map (lambda (i1 i2) (- i1 i2))
       lst1
       lst2))

(define (make-seq i j)
  (if (>= i j)
    (list)
    (cons i (make-seq (+ i 1) j))))

#| (define (nth lst n) |#
#|   (last (take lst n))) |#

(define (nth lst n)
  (list-ref lst (- n 1)))

(define (solve am)
  (define r (length am))
  (define c (length (car am)))
  (define (cancel pivot-row i)
    (lambda (row)
      (let* ((a (nth pivot-row i))
             (b (nth row i))
             (s (/ b a)))
        (list-sub row (scalar-mul s pivot-row)))))

  (define (reduce-fn1 i acc)
    (let* ((back (take acc i))
           (front (drop acc i))
           (back-end (last back))
           (new-front (map (cancel back-end i) front)))
      (append back new-front)))

  (define (reduce-fn2 i acc)
    (let* ((back (take acc (- i 1)))
           (front (drop acc (- i 1)))
           (front-start (car front))
           (new-back (map (cancel front-start i)
                          back)))
      (append new-back front)))

  (define triangle (foldl reduce-fn1 am (make-seq 1 r)))

  (define diagonal
    (foldl reduce-fn2
           triangle
           (reverse (make-seq 1 (+ r 1)))))

  (map (lambda (row i)
         (scalar-mul (/ 1 (nth row i)) row))
       diagonal
       (make-seq 1 (+ r 1))))

(define ex (list (list 1 -2 1 0)
                 (list 0 2 -8 8)
                 (list -4 5 9 -9)))

(define solved (solve ex))
(display solved)
(newline)
```  

### Extras
Copilot thinks this is poetry :)  
![a funny](media/lineq-gh-completion.png)
