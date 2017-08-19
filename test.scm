;; -*- coding: utf-8 -*-

(use gauche.test)
(test-start "advisor")
(use advisor)

(test-module 'advisor)

(define *stack* ())

(test-section "advice-add!/advice-remove!")
(define (base-func x y)
  (push! *stack* "base")
  (+ x y))

(define (dummy-advice x y) #f)

(advice-add! base-func :before dummy-advice)

(test* "it converts procedure into <advisable>" <advisable>
       (class-of base-func))

(advice-remove! base-func dummy-advice)
(test* "it converts <advisable> into original procedure" <procedure>
       (class-of base-func))

(test-section "advice-add! :before-if")

(define (advice-before-if x y)
  (even? x))

(advice-add! base-func :before-if advice-before-if)
(set! *stack* ())

(test* "it does nothing when :before-if returns false" #f
       (base-func 3 2))

(test* "the original function is called when :before-if returns true" 6
       (base-func 4 2))

(advice-remove! base-func advice-before-if)

(test-section "advice-add! :before")

(define (advice-before x y)
  (push! *stack* "before")
  #f)

(advice-add! base-func :before advice-before)
(set! *stack* ())

(test* ":before is called before the original function" '("base" "before")
       (begin
         (base-func 1 2)
         *stack*))
(test* ":before does not affect the result" 5
       (base-func 2 3))

(advice-remove! base-func advice-before)

(test-section "advice-add! :after")

(define (advice-after x y)
  (push! *stack* "after")
  "foo")

(advice-add! base-func :after advice-after)
(set! *stack* ())

(test* ":after is called after the original function" '("after" "base")
       (begin
         (base-func 3 4)
         *stack*))
(test* ":after does not affect the result" 5
       (base-func 3 2))

(advice-remove! base-func advice-after)

(test-end :exit-on-failure #t)
