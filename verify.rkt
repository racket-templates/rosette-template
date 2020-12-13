#lang rosette/safe

(require rosette/lib/angelic   ; for `choose*`
         rosette/lib/destruct) ; for `destruct` and `destruct*

;; A program is a list of instructions.
(struct neg (x) #:transparent)       ; regs[x] = -regs[x] ; x is an index
(struct add-val (x v) #:transparent) ; regs[x] += v       ; x is an index
                                     ;                      v is a value
(struct add-reg (x y) #:transparent) ; regs[x] += regs[y] ; x and y are indices

;; There are exactly 6 registers.
(define reg-size 6)

;; reg-size - 1 must fit in index?
(define index? (bitvector 4)) ; bitwidth = 4 (-8 to 7)
(define value? (bitvector 8)) ; bitwidth = 8 (-128 to 127)

(define (make-index idx) (bv idx index?))
(define (make-value val) (bv val value?))

;; assert that idx is a valid index
(define (assert-idx idx)
  (assert (and (bvule (make-index 0) idx)
               (bvult idx (make-index reg-size)))
          "index out of bound"))

;; we represent registers with a function that maps an index to a value
;; zero-regs is the initial state of the registers, which are zero-filled.
(define zero-regs (λ (idx)
                    (assert-idx idx)
                    (make-value 0)))

(define example-program                           ;  0  0  0  0  0  0
  (list (add-val (make-index 1) (make-value 4))   ;  0  4  0  0  0  0
        (add-val (make-index 0) (make-value 6))   ;  6  4  0  0  0  0
        (add-reg (make-index 3) (make-index 1))   ;  6  4  0  4  0  0
        (add-reg (make-index 3) (make-index 0))   ;  6  4  0 10  0  0
        (neg     (make-index 1))                  ;  6 -4  0 10  0  0
        (add-val (make-index 3) (make-value 3))   ;  6 -4  0 13  0  0
        (add-val (make-index 3) (make-value 2)))) ;  6 -4  0 15  0  0

;; An interpreter for our DSL. It consumes a list of instructions
;; and returns registers after the instructions are executed

(define (interp instrs)
  (let loop ([instrs instrs] [regs zero-regs])
    (destruct instrs
      [(list) regs]
      [(list current-instr rest-instrs ...)
       (loop rest-instrs
             (λ (idx)
               (assert-idx idx)
               ;; To update a register `x`, simply create a new mapping that
               ;; maps `x` to the new value and maps other indices to
               ;; the previous regs
               (destruct current-instr
                 [(neg x)
                  (if (bveq idx x)
                      (bvneg (regs x))
                      (regs idx))]
                 [(add-val x v)
                  (if (bveq idx x)
                      (bvadd (regs x) v)
                      (regs idx))]
                 [(add-reg x y)
                  (if (bveq idx x)
                      (bvadd (regs x) (regs y))
                      (regs idx))])))])))

;; print regs
(define (print-regs regs)
  (println
   (let loop ([i 0])
     (if (= i reg-size)
         '()
         (cons (regs (make-index i)) (loop (add1 i)))))))

(define (sep) (displayln "\n----------------\n"))

(displayln "The example (concrete) program")
example-program
(newline)
(displayln "An evaluation on the example program")
(print-regs (interp example-program))
(sep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here's a program optimization idea: for every consecutive add-val operating
;; on the same register, we can fuse them together. E.g.,
;;
;; (list (add-val (make-index 0) (make-value 1))
;;       (add-val (make-index 0) (make-value 2)))
;;
;; can be optimized to just one instruction:
;;
;; (list (add-val (make-index 0) (make-value 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; But what if we make a mistake? What if we accidentally perform this
;; optimization on add-reg instead? E.g.,
;;
;; (list (add-reg (make-index 0) (make-index 1))
;;       (add-val (make-index 0) (make-index 2)))
;;
;; is (incorrectly) optimized to
;;
;; (list (add-reg (make-index 0) (make-index 3)))

(define (xform-bad instrs)
  (destruct instrs
    [(list) (list)]
    [(list instr) (list instr)]
    [(list current-instr rest-instrs ...)
     ;; There are at least 2 instructions
     (define xformed-rest-instrs (xform-bad rest-instrs))
     (destruct xformed-rest-instrs
       [(list xformed-next-instr xformed-rest-rest ...)
        (destruct* (current-instr xformed-next-instr)
          [((add-reg x y) (add-reg x* y*))
           (cond
             ;; (incorrectly) optimize
             [(bveq x x*) (cons (add-reg x (bvadd y y*)) xformed-rest-rest)]
             ;; can't optimize
             [else (cons current-instr xformed-rest-instrs)])]
          [(_ _)
           ;; can't optimize
           (cons current-instr xformed-rest-instrs)])]
       ;; xformed-rest-instrs can't be empty
       [_ (assert #f "infeasible")])]))

(displayln "Incorrect optimization on the example program")
(xform-bad example-program)
(newline)
(displayln "An evaluation on the incorrectly optimized program")
(print-regs (interp (xform-bad example-program)))
(sep)

;; Compared the above with a correct optimization

(define (xform-good instrs)
  (destruct instrs
    [(list) (list)]
    [(list instr) (list instr)]
    [(list current-instr rest-instrs ...)
     ;; There are at least 2 instructions
     (define xformed-rest-instrs (xform-good rest-instrs))
     (destruct xformed-rest-instrs
       [(list xformed-next-instr xformed-rest-rest ...)
        (destruct* (current-instr xformed-next-instr)
          [((add-val x y) (add-val x* y*))
           (cond
             ;; optimize
             [(bveq x x*) (cons (add-val x (bvadd y y*)) xformed-rest-rest)]
             ;; can't optimize
             [else (cons current-instr xformed-rest-instrs)])]
          [(_ _)
           ;; can't optimize
           (cons current-instr xformed-rest-instrs)])]
       ;; xformed-rest-instrs can't be empty
       [_ (assert #f "infeasible")])]))


(displayln "Correct optimization on the example program")
(xform-good example-program)
(newline)
(displayln "An evaluation on the correctly optimized program")
(print-regs (interp (xform-good example-program)))
(sep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Roughly speaking, the optimization is correct when
;; (interp (xform instrs)) = (interp instrs)
;; for every possible instrs that are well-formed (e.g., no invalid reg index)
;; But that doesn't work because we can't compare functions
;; (which represent registers)
;;
;; What we actually want to say is, provided that idx are valid indices,
;; then it should be that ((interp (xform instrs)) idx) = ((interp instrs) idx)
;;
;; Here, we will verify that the above equation at 4 instructions

(define limit 4)

;; Generate a valid instruction
(define (symbolic-instr)
  ;; Each choice is a thunk. After a choice is picked, we immediately
  ;; apply the thunk to obtain the content inside the thunk.
  ((choose*
    (λ ()
      (define-symbolic* x index?)
      (assert-idx x)
      (neg x))
    (λ ()
      (define-symbolic* x index?)
      (define-symbolic* v value?)
      (assert-idx x)
      (add-val x v))
    (λ ()
      (define-symbolic* x y index?)
      (assert-idx x)
      (assert-idx y)
      (add-reg x y)))))

;; Generate a list of valid instructions
(define (symbolic-instrs limit)
  (cond
    [(zero? limit) '()]
    [else (cons (symbolic-instr) (symbolic-instrs (sub1 limit)))]))

(define (verify-xform xform)
  (clear-asserts!)

  (define instrs (symbolic-instrs limit))
  (define-symbolic idx index?)

  (define sol
    (time
     (verify
      #:assume (assert-idx idx)
      #:guarantee
      (assert (bveq ((interp instrs) idx) ((interp (xform instrs)) idx))))))

  ;; verification fails when either `((interp (xform instrs)) idx)` errors
  ;; or `(bveq ((interp instrs) idx) ((interp (xform instrs)) idx))` is `#f`
  ;; (assuming that `((interp instrs) idx)` doesn't error -- which should be
  ;;  the case since `instrs` is valid)

  (cond
    [(unsat? sol)
     (printf "~a is valid (up to the specified number of instructions)\n" xform)]
    [else
     (define instrs* (evaluate instrs sol))

     (printf "~a is invalid\n\n" xform)
     (printf "instrs:\n~a\n\n" instrs*)
     (printf "transformed instrs:\n~a\n\n" (xform instrs*))

     (printf "eval of unoptimized program:\n")
     (print-regs (interp instrs*))
     (newline)

     (printf "eval of incorrectly optimized program (could error):\n")
     (print-regs (interp (xform instrs*)))])
  (sep))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; expected verification time: 10s (on Apple M1)
(verify-xform xform-good)
;; expected verification time: 1s (on Apple M1)
(verify-xform xform-bad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
