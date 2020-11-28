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
    (with-output-to-port (current-error-port) (cute write msg))
    'wut))

(set! make-serialized-message
  (lambda (method arguments tag)
    ;(with-output-to-port (current-error-port) (cute print "HELLO FROM MAKE-SERIALIZED-MESSAGE"))
    (make-message method arguments tag)))

(test-group "transmission"
  ; TODO: The `format` field shouldn't be in the message.
  ; ((method . "torrent-get")
  ;  (arguments (fields . #("id"))
  ;             (ids . #())
  ;             (format . #f))
  ;  (tag . 0))
  (test-assert "Basic phony test" (torrent-get '("id") #:tag (unique-tag)))
  )

(test-group "transmission.utils"
  )

(test-exit)
