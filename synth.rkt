#lang rosette/safe

;; This example is taken from the blog post _Building a Program Synthesizer_
;; by James Bornholt
;; (https://www.cs.utexas.edu/~bornholt/post/building-synthesizer.html)

(require rosette/lib/angelic   ; provides `choose*`
         rosette/lib/destruct) ; provides `destruct`

;; Syntax for our simple DSL
;; Each subnode is either another node or an integer
(struct plus (left right) #:transparent)
(struct mul (left right) #:transparent)
(struct square (arg) #:transparent)

;; A simple concrete program
(define example-prog (plus (square 7) 3))

;; Interpreter for our DSL.
;; We just recurse on the program's syntax using pattern matching.
(define (interpret p)
  (destruct p
    [(plus a b)  (+ (interpret a) (interpret b))]
    [(mul a b)   (* (interpret a) (interpret b))]
    [(square a)  (define val (interpret a))
                 (* val val)]
    [_ p]))

(displayln ";; (plus (square 7) 3) evaluates to 52")
(interpret example-prog)

;; Create two symbolic integers `x` and `c`.
(define-symbolic x c integer?)

(displayln ";; Our interpreter works on symbolic values, too.")
(interpret (square (plus x 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(displayln ";; Find a `c` such that c*x = x+x for *every* x.")
(synthesize
  #:forall (list x)
  #:guarantee (assert (= (interpret (mul c x)) (+ x x))))

;; Create an unknown expression -- one that can evaluate to several
;; possible values.
(define (??expr terminals)
  (define a (apply choose* terminals))
  (define b (apply choose* terminals))
  (choose* (plus a b)
           (mul a b)
           (square a)
           a))

;; Create a sketch representing all programs of the form (plus ?? ??),
;; where the ??s are unknown expressions created by ??expr.
(define-symbolic p q integer?)
(define sketch
  (plus (??expr (list x p q)) (??expr (list x p q))))

;; Synthesize the sketch to find a program equivalent to 10*x,
;; but of the form (plus ?? ??). Save the resulting model.
(define M
  (synthesize
    #:forall (list x)
    #:guarantee (assert (= (interpret sketch) (interpret (mul 10 x))))))

(displayln ";; Substitute the bindings in M into the sketch to get back the")
(displayln ";; synthesized program.")
(evaluate sketch M)
