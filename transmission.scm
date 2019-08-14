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

   define-rpc-call
   )

  (import
    scheme
    (only chicken.base
          assert
          cut
          fixnum?
          identity
          make-parameter
          print
          when)
    (only chicken.condition
          condition-case
          get-condition-property
          signal)
    (only chicken.module
          export)
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

  (define (just x)     `(just . ,x))
  (define nothing      'nothing)
  (define (just? x)    (and (pair? x) (eq? (car x) 'just)))
  (define (nothing? x) (eq? x nothing))
  (define (maybe? x)   (or (nothing? x) (just? x)))
  (define (unwrap x)   (cdr x))
  (define (maybe f x)  (if (just? x) (just (f (unwrap x))) nothing))
  (define (->maybe b) (if b (just b) nothing))
  (define (maybe-do . procs)
    (lambda (x)
      (let loop ((x x) (procs procs))
        (cond
          ((null? procs) x)
          ((nothing? x) nothing)
          (else (loop ((car procs) (unwrap x)) (cdr procs)))))))

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
        (let ((optional (filter cdr `((arguments . ,arguments) (tag . ,tag)))))
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
                          (positive? (vector-length arguments))
                          arguments)))
      (and host url port
           (or (and username password)
               (not (or username password)))
           (call-int host url port username password method arguments tag))))

  ;;;
  ;;; General & common utilities
  ;;;

  ;; TODO: How to handle boolean values

  (define false? not)

  ; pre-proc :: x -> Maybe y
  (define (make-*->arguments key pre-proc)
    (lambda (x)
      (let ((x (pre-proc x)))
        (and (just? x) `(,key . ,(unwrap x))))))

  (define (pre-proc-id id)
    (if id
        (cond
          ((string? id)
           (just id))
          ((fixnum? id)
           (number->string id))
          (else nothing))
        nothing))

  ; ids :: Either String [ID]
  (define (pre-proc-ids ids)
    ((maybe-do
       ; Handle string ("recently-active") and list or vector of strings (hash)
       ;   and integers (id)
       (lambda (ids)
         (->maybe
           (cond
             ((or (string? ids)
                  (pair? ids))
              ids)
             ((and (vector? ids)
                   (positive? (vector-length ids)))
              (vector->list ids))
             (else #f))))

       (lambda (ids)
         (just (if (string? ids)
                   ids
                   (map unwrap (filter just? (map pre-proc-id ids)))))))

     (->maybe ids)))

  (define (pre-proc-list-of-strings strs)
    (when strs (for-each (lambda (str) (assert (string? str))) strs))
    (->maybe strs))

  (define pre-proc-fields pre-proc-list-of-strings)

  (define ids->arguments    (make-*->arguments 'ids    pre-proc-ids))
  (define fields->arguments (make-*->arguments 'fields pre-proc-fields))

  ;; TODO: [WIP] Basic definitions seem to work
  (define-syntax define-rpc-call
    (syntax-rules ()
      ((define-rpc-call (method (required required-handler) ...) (key default key-handler) ...)
       (begin
         (export method)
         (define method
           (let ((method-str (symbol->string 'method)))
             (lambda (required ... #!key (tag #f) (key default) ...)
               (let ((required (required-handler required)) ... (key (key-handler key)) ...)
                 (let ((arguments (list->vector (filter identity `(,required ... ,key ...)))))
                   (rpc-call method-str #:arguments arguments #:tag tag))))))))))

  (define-syntax define-3.1/4.6
    (syntax-rules ()
      ((define-3.1/4.6 method)
       (define-rpc-call (method) (ids #f ids->arguments)))))

  (define-syntax define-noargs
    (syntax-rules ()
      ((define-noargs method)
       (define-rpc-call (method)))))

  (define-rpc-call (torrent-get (fields fields->arguments)) (ids #f ids->arguments))
  (define-rpc-call (session-get) (fields #f fields->arguments) (ids #f ids->arguments))

  (define-noargs blocklist-update)
  (define-noargs session-stats)
  (define-noargs port-test)
  (define-noargs session-close)

  (define-3.1/4.6 queue-move-bottom)
  (define-3.1/4.6 queue-move-down)
  (define-3.1/4.6 queue-move-top)
  (define-3.1/4.6 queue-move-up)
  (define-3.1/4.6 torrent-reannounce)
  (define-3.1/4.6 torrent-start)
  (define-3.1/4.6 torrent-start-now)
  (define-3.1/4.6 torrent-stop)
  (define-3.1/4.6 torrent-verify)
  )
