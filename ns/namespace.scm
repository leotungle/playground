

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

(define (declarer ns-name fn-name fn-body)
  (list
   (list
    'define ns-name (list 'make-the-function))
   (list 'define (list 'create)
	 (list 'define (list fn-name) fn-body)
	 (list ns-name (list 'quote 'add) (list 'quote fn-name) fn-name))
   (list 'create)))

(define-syntax ns
  (syntax-rules (define)
    ((ns ns-name (define (fn-name) fn-body))
     (map eval (declarer 'ns-name 'fn-name 'fn-body)))))
		    
; (ns fooland (define (test) 'hey))

#; (ns barman
    (define (bong)
      (let loop ((x 0))
	(cond ((< x 10)
	       (begin (pp x) (loop (+ x 1))))
	      (else 'done)))))
