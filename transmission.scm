(module
  transmission
  (
   ;; Parameters
   *host*
   *password*
   *port*
   *session-id*
   *url*
   *username*

   ;; Core procedure
   rpc-call

   ;; 3.1 Torrent actions
   torrent-reannounce
   torrent-start
   torrent-start-now
   torrent-stop
   torrent-verify
   )

  (import
    scheme
    (only chicken.base
          assert
          cut
          fixnum?
          make-parameter
          print)
    (only chicken.condition
          condition-case
          get-condition-property
          signal)
    (only chicken.port
          with-output-to-string))

  (import
    (only http-client
          with-input-from-request)
    (only intarweb
          header-values
          headers
          make-request
          response-code
          response-headers)
    (only json
          json-read
          json-write)
    (only srfi-1
          filter)
    (only uri-common
          make-uri))

  (define (assert* loc type type?)
    (lambda (x)
      (assert (type? x) loc "Expected " type ", but got " x)
      x))

  (define (or? . rest)
    (lambda (x)
      (let loop ((l rest))
        (or (null? l)
            ((car l) x)
            (loop (cdr l))))))

  (define *host* (make-parameter "localhost" (assert* '*host* "a string" string?)))

  ;;; RPC Parameters
  ;;; @see https://github.com/transmission/transmission/wiki/Editing-Configuration-Files#rpc

  ;; rpc-url
  (define *url* (make-parameter '(/ "transmission" "rpc")))

  ;; rpc-port
  (define *port*
    (make-parameter 9091 (assert* '*port* "an integer" fixnum?)))

  ;; rpc-username
  (define *username*
    (make-parameter #f (assert* '*username* "a string or #f" (or? not string?))))

  ;; rpc-password
  (define *password*
    (make-parameter #f (assert* '*password* "a string or #f" (or? not string?))))

  ;; 2.3.1 X-Transmission-Session-Id
  ;; @see https://github.com/transmission/transmission/blob/master/extras/rpc-spec.txt
  (define *session-id*
    (make-parameter #f (assert* '*session-id* "a string or #f" (or? not string?))))

  (define (filter-arguments arguments)
    (filter (lambda (arg) (and arg (cdr arg))) arguments))

  ;; @brief Make an RPC call to a Transmission daemon
  ;; @param method A string naming an RPC method
  ;; @param arguments The arguments of this method
  ;; @param tag The tag for this call
  ;; @returns A response object read with json-read, or #f in case of wrong
  ;;          parameters
  ;;
  ;; Throws exceptions for HTTP errors, except for 409, which is handled
  ;;   according to 2.3.1; see http-client for other HTTP errors.
  ;;
  ;; `rpc-call` returns #f if parameters are wrong. Some parameters are
  ;; mandatory (with defaults provided): `*host*`, `*url*`, `*port*`.
  ;; Others are optional: `*session-id*`, `*password*`, `*username*`.
  ;; `*password*` and `*username*` are, however, optional "together"; they must
  ;; both be #f or a string.
  ;;
  ;; See the json egg for how to encode arguments and decode responses.
  (define (rpc-call method #!key (arguments #f) (tag #f))
    (define (make-req host url port username password)
      (let ((uri (make-uri #:scheme 'http
                           #:host host
                           #:port port
                           #:path url
                           #:username username
                           #:password password)))
        (make-request #:method 'POST
                      #:uri uri
                      ; NOTE: Not sure, but x-transmission-session-id set to #f
                      ;       seems to work
                      #:headers (headers `((x-transmission-session-id ,(*session-id*)))))))

    (define (make-content method arguments tag)
      (define (content-req method arguments tag)
        (let ((optional (filter-arguments `((arguments . ,arguments) (tag . ,tag)))))
          (list->vector `((method . ,method) . ,optional))))

      (with-output-to-string
        (cut json-write (content-req method arguments tag))))

    (define (call-int host url port username password method arguments tag)
      (let ((req (make-req host url port username password))
            (content (make-content method arguments tag)))

        (define (client-error-handler con)
          (let ((response (get-condition-property con 'client-error 'response)))

            ; See 2.3.1 for how to handle 409
            (cond

              ; The condition object may not have a resonse property, in which
              ;   case, it is not a 409
              ((and response
                    (= (response-code response) 409))
               (let ((session-id (car (header-values 'x-transmission-session-id (response-headers response)))))
                 ;(print "GOT A 409!")
                 (*session-id* session-id)
                 (let ((req (make-req host url port username password)))
                   (with-input-from-request req content json-read))))

              ; Rethrow any other errors
              (else
                ;(print "NOT A 409!")
                ; Is signal the right way?
                (signal con)))))

        (condition-case
          (with-input-from-request req content json-read)
          (con (exn http client-error) ; 409 is in this condition kind
               (client-error-handler con)))))

    (let ((host (*host*))
          (url (*url*))
          (port (*port*))
          (username (*username*))
          (password (*password*))
          (arguments (and arguments
                          (if (zero? (vector-length arguments))
                              #f
                              arguments))))
      (and host url port
           (or (and username password)
               (not (or username password)))
           (call-int host url port username password method arguments tag))))

  ;; TODO: Define convenience functions for common RPC calls

  ;; @brief Take an `ids` argument and return it ready to be embedded in the
  ;;        arguments vector
  ;; @param ids The `ids` argument as described in 3.1
  ;; @returns A pair `("ids" . ,ids) or #f
  ;; @see 3.1. Torrent Action Requests
  (define (ids->arguments ids)
    (let* ((ids (cond ((or (not ids) ; false?
                           (string? ids)
                           (pair? ids))
                       ids)
                      ((and (vector? ids)
                            (positive? (vector-length ids)))
                       (vector->list ids))
                      (else #f)))
           (ids (and ids
                     (if (string? ids)
                         ids
                         (map (lambda (x) (or (string->number x) x)) ids)))))
      (and ids `("ids" . ,ids))))

  ;;;
  ;;; 3.1 Torrent Actions
  ;;;

  (define-syntax define-3.1
    (syntax-rules ()
      ((define-3.1 method)
       (define method
         (let ((method-str (symbol->string 'method)))
           (lambda (#!key (tag #f) (ids #f))
             (let ((id-arguments (ids->arguments ids)))
               (let ((arguments (list->vector (filter-arguments `(,id-arguments)))))
                 (rpc-call method-str #:arguments arguments #:tag tag)))))))))

  (define-3.1 torrent-start)
  (define-3.1 torrent-start-now)
  (define-3.1 torrent-stop)
  (define-3.1 torrent-verify)
  (define-3.1 torrent-reannounce)
  )
