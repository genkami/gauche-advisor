;; -*- coding: utf-8 -*-

(use gauche.test)
(test-start "advisor")
(use advisor)

(test-module 'advisor)

(test-section "advice-add!/advice-remove!")
(define (base-func x y)
  (+ x y))

(define (dummy-advice x y) #f)

(advice-add! base-func :before dummy-advice)

(test* "it converts procedure into <advisable>" <advisable>
       (class-of base-func))

(advice-remove! base-func dummy-advice)
(test* "it converts <advisable> into original procedure" <procedure>
       (class-of base-func))

(test-end :exit-on-failure #t)
