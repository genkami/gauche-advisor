;; -*- coding: utf-8 -*-

(define-module advisor
  (use util.match)
  (export
   <advisable>
   make-advisable
   advice-add!
   %advice-add!
   advice-remove!
   %advice-remove!
   ))
(select-module advisor)

(define-class <advisable> ()
  (;; 元となる手続きもしくは callable
   [callable :init-keyword :callable]
   ;; callable の実行の周辺で呼ばれる手続きのリスト。
   [advices :init-value ()]
   ))

(define (make-advisable callable)
  (make <advisable> :callable callable))

;; Macro: convert-to-advisable-maybe! callable
;; callable が <advisable> のインスタンスでなかった場合、等価な((callable args ...) の返す
;; 値が等しい) <advisable> のインスタンスで callable の束縛を上書きする。
;; callable が <advisable> のインスタンスであった場合、何もしない。
(define-syntax convert-to-advisable-maybe!
  (syntax-rules ()
    [(_ callable)
     (when (not (is-a? callable <advisable>))
       (set! callable (make-advisable callable)))]
    ))

;; Macro: advice-add! advisable timing advice
;; advisable が <advisable> のインスタンスでなければ <advisable> のインスタンスに変換し、
;; %advice-add! により advice を追加する
(define-syntax advice-add!
  (syntax-rules ()
    [(_ advisable timing advice)
     (begin
       (convert-to-advisable-maybe! advisable)
       (%advice-add! advisable timing advice))]
    ))

;; Function: %advice-add! advisable timing advice
;; advisable の実行時に、指定した timing で手続き advice を実行する。
;; advice が受け取る引数、及び返すべき値は timing によって異なる。
;; timing は以下の値を取ることができる。
;;   + :around
;;        advisable の実行前に advice が呼ばれる。 advisable に渡された引数を
;;        args ... とすると、 advice には引数 advisable args ... が渡される。
;;        advice が rest-cont を実行しなかった場合、続きの処理は呼ばれない。
;;        また、 advice の返す値を全体の戻り値とする。
;;   + :before
;;        advisable の実行前に、 advisable に渡された引数をそのまま advice に渡す。
;;        advice の戻り値は捨てられる。
;;   + :after
;;        advisable の実行後に、 advisable に渡された引数をそのまま advice に渡す。
;;        advice の戻り値は捨てられ、 advisable の戻り値が全体の戻り値となる。
;;   + :override
;;        advisable の実行前に、 advisable に渡された引数をそのまま advice に渡す。
;;        advice の戻り値がそのまま返される。
;;   + :filter-args
;;        advisable の実行前に、 advisable に渡された引数をそのまま advice に返す。
;;        advice が(多値として)返した値を advisable の新たな引数にする。
;;   + :filter-return
;;        advisable の実行後に、 advisable が(多値として)返した値を引数に advice が呼ばれる。
;;        advice の戻り値がそのまま返される。
;;   + :before-if
;;        advisable の実行前に、 advisable に渡された引数をそのまま advice に渡す。
;;        advice の戻り値が真の値であった場合、同じ引数で advisable が呼ばれる。
;;        advice のもどちりが義の値であった場合、 advisable は呼ばれず、即座に #f が返される。
(define (%advice-add! advisable timing advice)
  (case timing
    [(:around :before :after :override :filter-args :filter-return :before-if)
     (set! (~ advisable 'advices) `((,timing . ,advice) . ,(~ advisable 'advices)))]
    [else (errorf "invalid timing: ~s" timing)]
    ))

;; Function: make-adviced-function proc timing advice return
;; proc をラップし、呼び出し時に timing で指定されるタイミングで advice を実行するような
;; 手続きを返す。
;; return は強制的にすべての手続きを終了させたい場合に呼ぶ継続。
;; timing と advice についての詳細は advice-add! を参照。
(define (make-adviced-function proc timing advice return)
  (case timing
    [(:around) (lambda args
                 (apply advice proc args))]
    [(:before) (lambda args
                 (apply advice args)
                 (apply proc args))]
    [(:after) (lambda args
                (let ([result (apply proc args)])
                  (apply advice args)
                  result))]
    [(:override) advice]
    [(:filter-args) (lambda args
                      (call-with-values (cut apply advice args) proc))]
    [(:filter-return) (lambda args
                        (call-with-values (cut apply proc args) advice))]
    [(:before-if) (lambda args
                    (if (apply advice args)
                        (apply proc args)
                        (return #f)))]
    [else (errorf "invalid timing: ~s" timing)]
    ))

;; Macro: advice-remove! advisable advice
;; %advice-remove! を用い、 advisable から advice を削除する。
;; advisable に一つも advice が残らなかった場合、 advisable をその callable で置き換える。
(define-syntax advice-remove!
  (syntax-rules ()
    [(_ advisable advice)
     (begin
       (%advice-remove! advisable advice)
       (when (null? (~ advisable 'advices))
         (set! advisable (~ advisable 'callable))))]
    ))

;; Function: %advice-remove! advisable advice
;; advisable から advice と eq? の意味で等しい advice をすべて削除する。
;; 等しい advice が存在しない場合は何もしない。
(define (%advice-remove! advisable advice)
  (set! (~ advisable 'advices)
        (filter (lambda (timing-and-advice)
                  (not (eq? advice (cdr timing-and-advice))))
                (~ advisable 'advices))))

;; Method: object-apply advisable . args
(define-method object-apply ([advisable <advisable>] . args)
  (let/cc return
    (define proc
      (let loop ([rest-cont (~ advisable 'callable)]
                 [advices (~ advisable 'advices)])
        (match advices
          [() rest-cont]
          [((timing . advice) . rest)
           (loop (make-adviced-function rest-cont timing advice return) rest)])
        ))
    (apply proc args)
    ))
