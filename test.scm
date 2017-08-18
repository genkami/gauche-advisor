;; -*- coding: utf-8 -*-

(use advisor)


(define (add x y)
  (format #t "add called x=~s y=~s\n" x y)
  (+ x y))

add
;; #<closure (add x y)>

(add 3 2)
;; add called x=3 y=2
;; 5

(define (add-before x y)
  (format #t ":before x=~s y=~s\n" x y))
(advice-add! add :before add-before)

add
;; #<<advisable> 0x10160d5e0>

(add 3 2)
;; :before x=3 y=2
;; add called x=3 y=2
;; 5

(define (add-after x y)
  (format #t ":after x=~s y=~s\n" x y))
(advice-add! add :after add-after)
(add 3 2)
;; :before x=3 y=2
;; add called x=3 y=2
;; :after x=3 y=2
;; 5

(define (add-override x y)
  (format #t ":override x=~s y=~s\n" x y)
  (* x y))
(advice-add! add :override add-override)
(add 3 2)
;; :before x=3 y=2
;; :override x=3 y=2
;; :after x=3 y=2
;; 6

(advice-remove! add add-override)
(add 3 2)
;; :before x=3 y=2
;; add called x=3 y=2
;; :after x=3 y=2
;; 5

(define (add-filter-args x y)
  (format #t ":filter-args x=~s y=~s\n" x y)
  (values (* x 2) (* y 2)))
(advice-add! add :filter-args add-filter-args)
(add 3 2)
;; :before x=3 y=2
;; :filter-args x=3 y=2
;; add called x=6 y=4
;; :after x=3 y=2
;; 10

(advice-remove! add add-filter-args)
(add 3 2)
;; :before x=3 y=2
;; add called x=3 y=2
;; :after x=3 y=2
;; 5

(define (add-filter-return ret)
  (format #t ":filter-return ret=~s\n" ret)
  (+ ret 10))
(advice-add! add :filter-return add-filter-return)
(add 3 2)
;; :before x=3 y=2
;; add called x=3 y=2
;; :filter-return ret=5
;; :after x=3 y=2
;; 15

(define (add-before-if x y)
  (format #t ":before-if x=~s y=~s\n" x y)
  (even? x))
(advice-add! add :before-if add-before-if)
(add 3 2)
;; :before x=3 y=2
;; :before-if x=3 y=2
;; #f

(add 4 2)
;; :before x=4 y=2
;; :before-if x=4 y=2
;; add called x=4 y=2
;; :filter-return ret=6
;; :after x=4 y=2
;; 16

(advice-remove! add add-before)
(advice-remove! add add-after)
(advice-remove! add add-filter-return)
(advice-remove! add add-before-if)
add
;; #<closure (add x y)>
