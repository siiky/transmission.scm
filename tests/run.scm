(import chicken.port)
(import test)
(import transmission transmission.utils)

(set! http-call
  ; `req`, created in `make-rpc-request`, contains the HTTP related parameters:
  ;   HTTP method, hostname, port, URL, username, password, and headers.
  ;
  ; `msg` is the body of the HTTP request to be made to the server, as a
  ;   string.
  (lambda (req msg)
    (with-output-to-port (current-error-port) (lambda () (print message)))
    #t))

;(set! rpc-call
;  (lambda (method #!key (arguments #f) (tag #f))
;    (with-output-to-port (current-error-port) (lambda () (print method)))
;    #t))

(test-group "transmission"
  (test "Basic `rpc-call` test" #t (torrent-get '("id") #:tag (unique-tag)))
  )

(test-group "transmission.utils"
  (test "Basic `rpc-call` test" #t (torrent-get '("id") #:tag (unique-tag)))
  )

(test-exit)
