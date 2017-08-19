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
   advisable->proc
   ))
(select-module advisor)

;; Class: <advisable>
(define-class <advisable> ()
  ([callable :init-keyword :callable]
   [advices :init-form (make-hash-table 'eq?)]
   ))

;; Function: make-advisable callable
(define (make-advisable callable)
  (let ([advisable (make <advisable> :callable callable)])
    (dolist (pointcut *pointcuts*)
      (hash-table-put! (~ advisable 'advices) pointcut ()))
    advisable))

;; Variable: *pointcuts*
;; A list of all pointcuts arranged in a precedence order. See %advice-add! for details.
(define *pointcuts*
  '(:override :around :filter-return :filter-args :after :before :before-if))

;; Macro: advice-add! advisable pointcut advice
;; This macro converts advisable into an equivalent instance of <advisable> when
;; advisable is not an instance of <advisable>, then it adds advice to advisable.
;; See %advice-add! for details.
(define-syntax advice-add!
  (syntax-rules ()
    [(_ advisable pointcut advice)
     (begin
       (convert-to-advisable-maybe! advisable)
       (%advice-add! advisable pointcut advice))]
    ))

(define-syntax convert-to-advisable-maybe!
  (syntax-rules ()
    [(_ callable)
     (when (not (is-a? callable <advisable>))
       (set! callable (make-advisable callable)))]
    ))

;; Function: %advice-add! advisable pointcut advice
;; This function adds advice to advisable. You can specify following values as pointcut.
;; The meaning of pointcut is as follows:
;; + :around
;;     Advice is called before calling advisable. Advice takes arguments orig args ... where
;;     orig is the rest part of entire function call and args is the original arguments
;;     passed to advisable. Note that args may be modified by other advices.
;;     Advisable directly returns the value returned by advice (or that overwitten by other
;;     advices) so if advice does not call (orig args ...) the call of advisable immediately
;;     stops and the rest part of entire function call is ignored.
;; + :before
;;     Advice is called before calling advisable with the same arguments of advisable
;;     (possibly overwriten by other advices). The returning value of advice is ignored
;;     and the next part of entire function call is always executed.
;; + :after
;;     Advice is called after calling advisable with the same arguments of advisable.
;;     The returning value of advice is ignored.
;; + :override
;;     Advice overrides the original function of advisable, that is, advice is called with
;;     the original arguments of advisable and the returning value of advice becomes the
;;     result of entire function call. Note that both arguments and returning values may
;;     be overwritten by other advices.
;; + :filter-args
;;     Advice is called with the original arguments of advisable and the returning values
;;     of advice (as multiple values) are passed by the rest part of function call.
;; + :filter-return
;;     Advice is called with the returning values of advisable (as multiple values) and
;;     the returning values of advice becomes the result of entire function call.
;; + :before-if
;;     Advice is called with the original arguments of advisable and it determines whether
;;     the rest part of function call is executed. The rest part is called with the same
;;     arguments when advice returns true. Otherwise the execution stops and it immediately
;;     returns #f.
(define (%advice-add! advisable pointcut advice)
  (if (memq pointcut *pointcuts*)
      (hash-table-update! (~ advisable 'advices) pointcut
                          (lambda (advices) (cons advice advices)))
      (errorf "invalid pointcut: ~s" pointcut)))

;; Macro: advice-remove! advisable advice :optional pointcut
;; This macro removes advice from advisable by %advice-remove!. If advisable has no more
;; advices, then it is converted into an equivalent callable.
(define-syntax advice-remove!
  (syntax-rules ()
    [(_ advisable advice pointcut ...)
     (begin
       (%advice-remove! advisable advice pointcut ...)
       (when (advisable-has-no-advice? advisable)
         (set! advisable (~ advisable 'callable))))]
    ))

(define (advisable-has-no-advice? advisable)
  (fold-left (lambda (result pointcut)
               (and result (null? (~ advisable 'advices pointcut))))
             #t
             *pointcuts*))

;; Function: %advice-remove! advisable advice :optional pointcut
;; This function removes advice associated with pointcut from advisable. If pointcut is not
;; specified, then it removes advisable associated with any pointcuts.
(define (%advice-remove! advisable advice :optional (pointcut #f))
  (define (remove-from! pointcut)
    (hash-table-update! (~ advisable 'advices) pointcut
                        (lambda (advices)
                          (filter (lambda (a) (not (eq? advice a))) advices))))
  (if pointcut
      (if (memq pointcut *pointcuts*)
          (remove-from! pointcut)
          (errorf "invalid pointcut: ~s" pointcut))
      (dolist (pointcut *pointcuts*)
        (remove-from! pointcut))))

;; Method: object-apply advisable . args
;; This method calls original callable and all advices in specified pointcuts.
;; The order of advice calls are as follows:
;; 1. First, all advices associated with :before-if are called with args. When one of them
;;    returns false, then execusion stops.
;; 2. Then, all advices associated with :before are called with args.
;; 3. Then,  all advices associated with :filter-args are called. Args is passed to the first
;;    advice of them and the returning values of it is passed to the second advice, and so on.
;;    The returning values of the last advice is passed to the rest of function call.
;; 4. Then, all advices associated with :around is called. The first advice of them is called
;;    with the rest part of execusion and the returning value of the last advice associated
;;    with :filter-args. If an advice does not call the rest part, then the rest advices
;;    associated with :around or :override and the original function of advisable is ignored
;;    and the executing process immediately jumps to step 6.
;; 5. Then, an advice associated with :override is called if exists. Otherwise the original
;;    function of advisable is called. When there are more than one advices associated with
;;    :override, only the last one is called.
;; 6. Then, all advices associated with :filter-return is called. The first advice is called
;;    with the returning value (as multiple values) of an advice associated with :override
;;    or the original function of advisable. Similarly, the n-th advice is called with the
;;    returning values of the (n-1)-th advice. The returning value of the last advice is the
;;    result of entire executing process.
;; 7. Finally, all advices associated with :before is called with *the original argument*.
;;    Note that they are always called unless advices associated with :before-if return false.
(define-method object-apply ([advisable <advisable>] . args)
  (apply (advisable->proc advisable) args))

;; Function: advisable->proc advisable
;; Converts advisable into equivalent procedure.
(define (advisable->proc advisable)
  (fold-left (lambda (proc pointcut)
               (fold-left (lambda (proc advice)
                            (make-adviced-function proc pointcut advice))
                          proc
                          (reverse (~ advisable 'advices pointcut))))
             (~ advisable 'callable)
             *pointcuts*))

(define (make-adviced-function proc pointcut advice)
  (case pointcut
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
                        #f))]
    [else (errorf "invalid pointcut: ~s" pointcut)]
    ))
