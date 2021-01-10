(defstruct connection-options
           host
           port
           authenv
           username
           password
           rest)

(define-syntax eprint
  (syntax-rules ()
    ((eprint arg ...)
     (with-output-to-port (current-output-port) (lambda () (print arg ...))))))

(define *connection-opts*
  (cling
    (lambda (ret . rest)
      (update-connection-options ret #:rest (cadr rest)))

    (arg '((--host) . host)
         #:help "The host of the transmission instance"
         #:kons (lambda (ret _ host) (update-connection-options ret #:host host)))

    (arg '((--port) . port)
         #:help "The port of the transmission instance"
         #:kons (lambda (ret _ port) (update-connection-options ret #:port port)))

    (arg '((--username) . username)
         #:help "The username to access the transmission instance"
         #:kons (lambda (ret _ username) (update-connection-options ret #:username username)))

    (arg '((--password) . password)
         #:help "The password to access the transmission instance"
         #:kons (lambda (ret _ password) (update-connection-options ret #:password password)))

    (arg '((--authenv -ne))
         #:help "Like transmission-remote, use the environment variable TR_AUTH"
         #:kons (lambda (ret _ _) (update-connection-options ret #:authenv #t)))))

(define (set-parameters! #!key host port authenv username password)
  (when host (*host* host))
  (when port (*port* port))

  ; Update authentication variables
  (cond
    (authenv
      (let ((tr_auth (get-environment-variable "TR_AUTH")))
        (unless tr_auth
          (eprint "TR_AUTH not set"))
        (let ((username/password (string-split tr_auth ":")))
          (unless (= (length username/password) 2)
            (eprint "Splitting TR_AUTH by ':' didn't result in 2 elements; authentication will most likely fail."))
          (let ((username (car username/password))
                (password (cadr username/password)))
            (*username* username)
            (*password* password)))))

    ((or username password)
     (unless (and username password)
       (eprint "Did you forget to set both username and password? Authentication may fail."))
     (when username (*username* username))
     (when password (*password* password)))
    (else #f))
  #t)

(define (set-connection-options! #!optional (args (command-line-arguments)))
  (let ((options (process-arguments *connection-opts* (make-connection-options) args)))
    (set-parameters!
      #:host (connection-options-host options)
      #:port (connection-options-port options)
      #:authenv (connection-options-authenv options)
      #:username (connection-options-username options)
      #:password (connection-options-password options))
    (values (connection-options-rest options) options)))
