

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
