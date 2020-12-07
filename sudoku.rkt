#lang rosette/safe

;; An example game from https://en.wikipedia.org/wiki/Sudoku
;; 0 means blank and is to be filled
(define game '([5 3 0 0 7 0 0 0 0]
               [6 0 0 1 9 5 0 0 0]
               [0 9 8 0 0 0 0 6 0]
               [8 0 0 0 6 0 0 0 3]
               [4 0 0 8 0 3 0 0 1]
               [7 0 0 0 2 0 0 0 6]
               [0 6 0 0 0 0 2 8 0]
               [0 0 0 4 1 9 0 0 5]
               [0 0 0 0 8 0 0 7 9]))

;; every cell is between 1 and 9
(define game*
  (map (λ (row)
         (map (λ (cell)
                (cond
                  [(zero? cell)
                   (define-symbolic* x integer?)
                   (assert (<= 1 x 9))
                   x]
                  [else cell]))
              row))
       game))

;; every row has distinct numbers
(for-each (λ (row) (assert (apply distinct? row))) game*)

;; every column has distinct numbers
;; = every row in the transposed game has distinct numbers

(define (transpose xs) (apply map list xs))

(for-each (λ (row) (assert (apply distinct? row))) (transpose game*))

;; every "group" has distinct numbers

(define (get-cell x y) (list-ref (list-ref game* y) x))

(define (get-group x y)
  (list (get-cell (+ (* 3 x) 0) (+ (* 3 y) 0))
        (get-cell (+ (* 3 x) 1) (+ (* 3 y) 0))
        (get-cell (+ (* 3 x) 2) (+ (* 3 y) 0))
        (get-cell (+ (* 3 x) 0) (+ (* 3 y) 1))
        (get-cell (+ (* 3 x) 1) (+ (* 3 y) 1))
        (get-cell (+ (* 3 x) 2) (+ (* 3 y) 1))
        (get-cell (+ (* 3 x) 0) (+ (* 3 y) 2))
        (get-cell (+ (* 3 x) 1) (+ (* 3 y) 2))
        (get-cell (+ (* 3 x) 2) (+ (* 3 y) 2))))

(assert (apply distinct? (get-group 0 0)))
(assert (apply distinct? (get-group 1 0)))
(assert (apply distinct? (get-group 2 0)))
(assert (apply distinct? (get-group 0 1)))
(assert (apply distinct? (get-group 1 1)))
(assert (apply distinct? (get-group 2 1)))
(assert (apply distinct? (get-group 0 2)))
(assert (apply distinct? (get-group 1 2)))
(assert (apply distinct? (get-group 2 2)))

(define sol (solve #t))
(displayln "solution:")
(for-each println (evaluate game* sol))
