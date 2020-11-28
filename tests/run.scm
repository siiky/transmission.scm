(import chicken.port)
(import test)
(import transmission transmission.utils)

(define-syntax define!
  (syntax-rules ()
    ((define! (func arg ...) body ...)
     (set! func (lambda (arg ...) body ...)))))

(define! (http-call req msg)
         'wut)

(set! make-serialized-message make-message)

(test-group "transmission"
  (test-assert "Basic phony test" (torrent-get '("id") #:tag (unique-tag)))
  )

(test-group "transmission.utils"
  )

(test-exit)
