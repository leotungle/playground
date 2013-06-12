

(define (make-the-function)
  (let ((fns (list)))
    (lambda (cmd . args)
      (cond ((eqv? cmd 'dump) fns)
            ((eqv? cmd 'add)
             (let ((name (car args))
                   (fn (cadr args)))
               (set! fns (cons (list name fn) fns))))
            (else
             (let* ((name-fn (assv cmd fns))
                    (fn (if name-fn
                            (cadr name-fn)
                            #f)))
               (if fn (apply fn args) 'no-function)))))))

(define (declarer ns-name fn-sig fn-body)
  (list
   (list
    'define ns-name (list 'make-the-function))
   (list 'define (list 'create)
         (list 'define fn-sig fn-body)
         (list ns-name (list 'quote 'add) (list 'quote (car fn-sig)) (car fn-sig)))
   (list 'create)))

(define-syntax ns
  (syntax-rules (define)
    ((ns ns-name (define (fn-name ...) fn-body))
     (map eval (declarer 'ns-name '(fn-name ...) (quote fn-body))))
    ((ns ns-name (define (fn-name ...) fn-body ...))
     (ns ns-name (define (fn-name ...) (begin fn-body ...))))))

(define (do-stuff . x)
  (apply list x))

(define-syntax delims
  (syntax-rules (foo)
    ((delims delim (foo elem) ...)
     (list delim (do-stuff elem ...) delim))))

(define-syntax delims2
  (syntax-rules (foo)
    ((delims ns-name (foo fn-name fn-body) ...)
     (list
      (list 'foo (list 'ns-name 'arg) (list 'quote 'hello))
      (do-stuff fn-name ... fn-body ...)
      (list 'foo (list 'end) (list 'its 'over))))))

(define-syntax delims3
  (syntax-rules (foo)
    ((delims ns-name (foo (fn-name ...) fn-body ...) ...)
     (list
      (list 'foo 'ns-name (list 'make-the-function))
      (list 'foo (list 'create)
            (list 'foo (list fn-name ...) fn-body ...)
            ;(list 'ns-name (list 'quote 'add) (list 'quote 'fn-name) 'fn-name)
            ...)
      (list 'create)))))

(define (add ns-name . fn-sig)
  (list ns-name
        (list 'quote 'add)
        (list 'quote (car fn-sig))
        (car fn-sig)))

(define-syntax delims4
  (syntax-rules (foo)
    ((delims ns-name (foo (fn-name ...) fn-body ...) ...)
     (list
      (list 'foo 'ns-name (list 'make-the-function))
      (list 'foo (list 'create)
            (list 'foo (list fn-name ...) fn-body ...) ...
            (add 'ns-name fn-name ...)
            ...)
      (list 'create)))))

; (delims4 'test (foo ('n1 'n2) 'b1 'b2) (foo ('n3) 'b3))

; (ns fooland (define (test) 'hey))

#; (ns barman
    (define (bong)
      (let loop ((x 0))
        (cond ((< x 10)
               (begin (pp x) (loop (+ x 1))))
              (else 'done)))))

#; (ns donkey
      (define (mule a b)
        (define (pow a)
          (let loop ((i 0) (x 1))
            (cond ((< i a) (loop (+ i 1) (* x 2)))
                  (else x))))
        (+ (pow a) (pow b))))
