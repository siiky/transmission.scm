(module transmission
  (
   *host*
   *password*
   *port*
   *session-id*
   *url*
   *username*

   deserialize-message
   serialize-message

   reply-arguments
   reply-ref
   reply-result
   reply-success?
   reply-tag

   default-error-proc
   exception->result
   result-bind
   result-ref
   result-ref*
   result/error
   result/error-ref
   result/error?
   result/ok
   result/ok-ref
   result/ok?
   result?
   with-transmission-result

   handle-409
   http-call
   make-message
   make-rpc-request
   make-serialized-message
   update-request-session-id

   rpc-call

   make-rpc-call
   define-rpc-call
   export-rpc-call

   torrent-source/filename
   torrent-source/filename?
   torrent-source/metainfo
   torrent-source/metainfo?
   torrent-source?

   format?
   id?
   ids?
   )

  (import
    scheme
    (only chicken.base
          alist-ref
          and-let*
          assert
          compose
          constantly
          current-error-port
          cute
          declare
          define-constant
          error
          fixnum?
          gensym
          identity
          make-parameter
          o)
    (only chicken.condition
          condition-case
          get-condition-property
          print-error-message
          signal)
    (only chicken.module
          export)
    (only chicken.port
          with-output-to-port)
    (only chicken.string
          ->string))

  (import
    (only http-client
          with-input-from-request)
    (only intarweb
          header-value
          headers
          make-request
          response-code
          response-headers
          update-request)
    ; NOTE: I'm not too comfortable with medea deserializing objects into
    ;       alists with symbols as keys, but I assume no one will use this to
    ;       communicate with a transmission instance they don't own, and thus
    ;       possibly malicious...
    (only medea
          json->string
          read-json)
    (only srfi-1
          any
          every
          filter)
    (only uri-common
          make-uri))

  ; NOTE: Disable inlining for these functions, so that they can be overwritten
  ;       in the tests. I don't quite understand the difference between
  ;       `inline` and `inline-global`... `inline` is supposed to control
  ;       inlining inside of the module, which is what we need, and
  ;       `inline-global` is supposed to control inlining across modules; but
  ;       they both seem to do the job for the tests.
  ; @see https://wiki.call-cc.org/man/5/Declarations#inline
  (declare
    (not inline
         http-call
         make-serialized-message))

  (import
    (rename
      (only srfi-189
            either-bind
            either-ref
            either?
            exception->either
            left
            left?
            right
            right?)
      (either-bind       result-bind)
      (either-ref        result-ref)
      (either?           result?)
      (exception->either exception->result)
      (left              result/error)
      (left?             result/error?)
      (right             result/ok)
      (right?            result/ok?)))

  (define false (constantly #f))
  (define true  (constantly #t))

  (define (result-ref* result)
    (result-ref result values values))

  (define (result/error-ref result #!optional (fail false))
    (result-ref result values fail))

  (define (result/ok-ref result #!optional (fail false))
    (result-ref result fail values))

  (define (default-error-proc result/con #!optional tag req resp)
    (if req
        (let ((msg (string-append
                     "RPC call "
                     (if (fixnum? tag)
                         (string-append "with tag " (number->string tag))
                         "")
                     " failed with the following error")))
          (error 'with-transmission-result msg result/con))

        (with-output-to-port
          (current-error-port)
          (lambda ()
            (print-error-message 'with-transmission-result result/con)))))

  ; NOTE: The same as result-ref, except the success and failure procedures are
  ;       flipped.
  (define (with-transmission-result result success-proc #!optional (error-proc default-error-proc))
    (let ((error-proc
            (if (eq? error-proc default-error-proc)
                error-proc
                (lambda (result/con #!optional tag req resp)
                  (error-proc result/con tag req resp)))))
      (result-ref result error-proc success-proc)))

  (define ((assert* loc type type?) x)
    (assert
      (type? x) loc
      (string-append "Expected " type ", but got " (->string x)))
    x)

  (define false? not)
  (define (->bool x) (not (not x)))

  (define ((or? . rest) x)
    (any (cute <> x) rest))

  (define reply-ref alist-ref)
  (define (reply-result reply)    (reply-ref 'result reply))
  (define (reply-arguments reply) (reply-ref 'arguments reply))
  (define (reply-tag reply)       (reply-ref 'tag reply))

  (define (reply-result-success? result)
    (and (string? result) (string=? result "success")))

  (define (reply-success? reply)
    (and reply (reply-result-success? (reply-result reply))))

  ;;;
  ;;; RPC Parameters
  ;;; @see https://github.com/transmission/transmission/wiki/Editing-Configuration-Files#rpc
  ;;;

  (define *host* (make-parameter "localhost" (assert* '*host* "a string" string?)))

  ;; rpc-url
  (define *url* (make-parameter '(/ "transmission" "rpc")))

  ;; rpc-port
  (define *port* (make-parameter 9091 (assert* '*port* "an integer" fixnum?)))

  ;; rpc-username
  (define *username*
    (make-parameter #f (assert* '*username* "a string or #f" (or? false? string?))))

  ;; rpc-password
  (define *password*
    (make-parameter #f (assert* '*password* "a string or #f" (or? false? string?))))

  ;; 2.3.1 X-Transmission-Session-Id
  ;; @see https://github.com/transmission/transmission/blob/master/extras/rpc-spec.txt
  (define *session-id* (make-parameter "" (assert* '*session-id* "a string" string?)))

  ;;;
  ;;; Core Procedures
  ;;;

  (define serialize-message json->string)
  (define deserialize-message read-json)

  ;; @brief Create a request object.
  ;; @param host The server's hostname.
  ;; @param url See `rpc-url`.
  ;; @param port See `rpc-port`.
  ;; @param username See `rpc-username`.
  ;; @param password See `rpc-password`.
  ;; @param session-id The `x-transmission-session-id` header.
  ;; @returns A request object.
  ;;
  ;; @a username and @a password are only required if
  ;;   `rpc-authentication-required` is enabled in the server.
  (define (make-rpc-request host url port username password #!optional (session-id (*session-id*)))
    (make-request
      #:method 'POST
      #:uri (make-uri
              #:scheme 'http
              #:host host
              #:port port
              #:path url
              #:username username
              #:password password)
      #:headers (headers `((x-transmission-session-id ,session-id)))))

  ;; @brief Create a Scheme representation of a message, according to the spec.
  ;; @param method The `method` field.
  ;; @param arguments The `arguments` field.
  ;; @param tag The `tag` field.
  ;; @returns A Scheme object representing a message.
  ;; @see Section 2.1.
  (define (make-message method arguments tag)
    (let ((optional (filter cdr `((arguments . ,arguments) (tag . ,tag)))))
      `((method . ,method) . ,optional)))

  ;; @brief Create and serialize a message, according to the spec.
  ;; @param method The `method` field.
  ;; @param arguments The `arguments` field.
  ;; @param tag The `tag` field.
  ;; @returns A string with the serialized JSON message.
  ;; @see Section 2.1.
  (define make-serialized-message (compose serialize-message make-message))

  ;; @brief Update a request's `x-transmission-session-id` header.
  ;; @param request The request.
  ;; @param session-id The new session ID.
  ;; @returns The updated request.
  (define (update-request-session-id request #!optional (session-id (*session-id*)))
    (update-request request #:headers (headers `((x-transmission-session-id ,session-id)))))

  ;; @brief Actually make the HTTP request to the server.
  ;; @param request The HTTP request object.
  ;; @param message The body of the HTTP request.
  ;; @returns The deserialized response of the RPC call.
  (define (http-call request message)
    (with-input-from-request request message deserialize-message))

  ;; @brief Handle 409, according to the spec.
  ;; @param condition The condition object.
  ;; @param request The request.
  ;; @param message The RPC message.
  ;; @returns The deserialized response of the RPC call.
  ;;
  ;; If the condition is caused by anything other than 409, the exception is
  ;;   propagated. In case of 409, the new session ID is read from the response
  ;;   headers and we try again. If this second try results in an error,
  ;;   it is propagated.
  (define (handle-409 condition request message)
    (let ((response (get-condition-property condition 'client-error 'response)))

      ; See 2.3.1 for how to handle 409
      ; The condition object may not have a response property, in which
      ;   case, it is not a 409
      (if (and response (= (response-code response) 409))

          (let ((session-id (header-value 'x-transmission-session-id (response-headers response) "")))
            (*session-id* session-id)
            (let ((req (update-request-session-id request session-id)))
              (http-call req message)))

          ; Rethrow any other errors
          (signal condition))))

  ;; @brief Make an RPC call to a Transmission daemon.
  ;; @param method A string naming an RPC method.
  ;; @param arguments The arguments of this method.
  ;; @param tag The tag for this call.
  ;; @returns A response object read with deserialize-message, or #f in case of
  ;;          wrong parameters
  ;;
  ;; Throws exceptions for HTTP errors, except for 409, which is handled
  ;;   according to 2.3.1; see http-client for other HTTP errors.
  ;;
  ;; `rpc-call` returns #f if parameters are wrong. Some parameters are
  ;;   mandatory (with defaults provided): `*host*`, `*url*`, `*port*`.
  ;;   Others are optional: `*session-id*`, `*password*`, `*username*`.
  ;;   `*password*` and `*username*` are, however, optional "together"
  ;;   -- they must both be #f or a string.
  ;;
  ;; See the medea egg for how to encode arguments and decode responses.
  (define (rpc-call method #!key (arguments #f) (tag #f))
    (define (call host url port username password method arguments tag)
      (let ((req (make-rpc-request host url port username password))
            (message (make-serialized-message method arguments tag)))
        (result-ref
          (exception->result
            true
            (lambda ()
              (condition-case (http-call req message)
                (condition (exn http client-error) ; 409 is in this condition kind
                           (handle-409 condition req message)))))
          result/error
          ; @param reply The Transmission daemon's reply message.
          ; @param req The URI-common request object.
          ; @param resp An intarweb#response object.
          (lambda (reply req resp)
            (if (reply-success? reply)
                (result/ok (reply-arguments reply) (reply-tag reply) req resp)
                (result/error (reply-result reply) (reply-tag reply) req resp))))))

    (define (!xor a b) (or (and a b) (not (or a b))))
    (and-let*
      ((host (*host*))
       (url (*url*))
       (port (*port*)))
      (let ((username (*username*))
            (password (*password*))
            (arguments (and arguments (not (null? arguments)) arguments)))
        ; NOTE: The spec (2.1, (3)) says `tag` is a number, but doesn't specify
        ;       if it must be an integer. The server returns #f as the `tag`
        ;       when a float is sent, so I think only integers are legal.
        (and ((or? false? fixnum?) tag)
             (!xor username password)
             (call host url port username password method arguments tag)))))

  ;;;
  ;;; General & Common Utilities
  ;;;

  ; TODO: Take a look at SRFI-189.
  (define-constant just-sym (gensym 'just))
  (define-constant nothing  (gensym 'nothing))
  (define (just obj)     `(,just-sym . ,obj))
  (define (just? obj)    (and (pair? obj) (eq? (car obj) just-sym)))
  (define (nothing? obj) (eq? obj nothing))
  (define (maybe? obj)   ((or? nothing? just?) obj))
  (define (unwrap just)  (cdr just))

  (define (->maybe b)
    (cond (((or? false? nothing?) b) nothing)
          ((just? b) b)
          (else (just b))))

  ;; maybe :: (a -> Maybe b) -> Maybe a -> Maybe b
  (define (maybe f x)
    (cond
      ((just? x) (f (unwrap x)))
      ((nothing? x) nothing)
      (else (f x))))

  ;; maybe-map :: (a -> b) -> Maybe a -> Maybe b
  (define (maybe-map f x)
    (maybe (o just f) x))

  ;; For `torrent-add`
  ; NOTE: The 'filename and 'metainfo symbols are important, do not change
  ;       them! They're used as the key in the arguments object.
  ; NOTE: Using Either from SRFI-189 would involve more work than the current
  ;       implementation.
  (define (torrent-source/filename str) (and (string? str) `(filename . ,str)))
  (define (torrent-source/metainfo str) (and (string? str) `(metainfo . ,str)))
  (define ((torrent-source-with-tag? tag) obj)
    (and (pair? obj) (eq? tag (car obj)) (string? (cdr obj))))
  (define torrent-source/filename? (torrent-source-with-tag? 'filename))
  (define torrent-source/metainfo? (torrent-source-with-tag? 'metainfo))
  (define (torrent-source? obj)
    (and (pair? obj)
         (or (eq? (car obj) 'filename)
             (eq? (car obj) 'metainfo))
         (string? (cdr obj))))

  ;; @brief Make a function that returns #f or an argument pair.
  ;; @param key The argument's key.
  ;; @param proc Function that processes an input value.
  ;; @returns A function that, from an input value, returns #f or an argument
  ;;          pair.
  ;;
  ;; @a proc must return a Maybe. If the result of proc is Nothing, it returns
  ;;   #f, and if the result is a Just, it returns an argument pair.
  (define ((make-*->argument key proc) x)
    (let ((x (proc x)))
      (and (just? x) `(,key . ,(unwrap x)))))

  ; TODO: Strict version
  (define proc-object ->maybe)

  (define (format? obj)
    (case obj
      (("objects" "table") #t)
      (else #f)))

  (define format->argument
    (make-*->argument
      'format
      (lambda (format)
        (if (format? format)
            (just format)
            nothing))))

  (define (id? id)
    (or (fixnum? id)
        (and (string? id)
             (or (= (string-length id) 40) ; SHA1
                 (string=? id "recently-active")))))

  (define (ids? ids)
    (or (id? ids)
        (and (list? ids) (every id? ids))
        (and (vector? ids) (every id? (vector->list ids)))))

  ;; @brief Pre-process a list of IDs.
  ;; @param ids A list of IDs.
  ;; @returns A Maybe.
  ;;
  ;; @a ids can be:
  ;;   * '() meaning no torrent (the default);
  ;;   * #f meaning all torrents;
  ;;   * "recently-active" meaning the recently active torrents;
  ;;   * a single ID (fixnum);
  ;;   * a single hash (string);
  ;;   * a list of torrent IDs and hashes
  (define ids->argument
    (make-*->argument
      'ids
      (lambda (ids)
        (define (proc-ids-list ids)
          ;; @brief Pre-process an ID.
          ;; @param id An ID.
          ;; @returns A Maybe.
          (define (proc-id id)
            (->maybe (and (id? id) id)))

          (let ((ids (map proc-id ids)))
            (if (every just? ids) (list->vector (map unwrap ids)) #())))

        (cond
          ((false? ids)
           nothing) ; Use all torrents if `ids` is ommited

          ((and (string? ids) (string=? ids "recently-active"))
           (just ids))

          ((list? ids)
           (just (proc-ids-list ids)))

          ((vector? ids)
           (just (proc-ids-list (vector->list ids))))

          ((id? ids)
           (just (proc-ids-list `(,ids))))

          (else (just #()))))))

  (define fields->argument
    (make-*->argument
      'fields
      (lambda (strs)
        (->maybe
          (and (list? strs)
               (every string? strs)
               (list->vector strs))))))

  (define (make-number->argument key)
    (make-*->argument
      key
      (lambda (n)
        ; NOTE: Haven't tested, but it may be that numbers must actually be
        ;       integers.
        (->maybe (and (number? n) n)))))

  (define (make-string->argument key)
    (make-*->argument
      key
      (lambda (str)
        (->maybe (and (string? str) str)))))

  (define (make-bool->argument key)
    (make-*->argument
      key
      (lambda (b)
        (if (nothing? b) nothing (just (->bool b))))))

  (define (make-array->argument key)
    (make-*->argument
      key
      (lambda (array)
        ; The medea egg serializes Scheme lists as JSON objects, and Scheme
        ;   vectors as JSON arrays.
        (cond
          ((vector? array) (just array))
          ((list? array) (just (list->vector array)))
          (else nothing)))))

  ; This is a slightly different case than the others. The others use
  ;   `make-*->argument` which assumes the processing function returns the
  ;   value, but not the key. In this case, both key and value are already in
  ;   the `ts` object.
  (define (torrent-source->argument ts)
    (and (torrent-source? ts) ts))

  ;;;
  ;;; Method Definitions
  ;;;

  ;; @brief Create a procedure for a single RPC method.
  ;; @param method The name of the RPC method.
  ;; @param required A required parameter.
  ;; @param required-handler A handler for a required parameter.
  ;; @param key A key parameter.
  ;; @param default The default value for a key parameter.
  ;; @param key-handler A handler for a key parameter.
  ;; @returns An RPC procedure.
  ;;
  ;; This macro creates an RPC procedure described by the given parameters.
  ;;
  ;; On top of the given key parameters, every RPC procedure has the `tag` key
  ;;   parameter, as per the RPC spec.
  ;;
  ;; All handlers (both required and key) must return either #f or a pair. This
  ;;   pair is the key and associated value of an RPC call argument.
  ;;
  ;; Examples:
  ;;
  ;; A call of no arguments (other than `tag`):
  ;;   (make-rpc-call some-method)
  ;;
  ;; A call with one required argument `fields` and one key argument `ids`:
  ;;   (make-rpc-call (some-method (fields fields->argument)) (ids '() ids->argument))
  (define-syntax make-rpc-call
    (syntax-rules ()
      ((make-rpc-call (method (required required-handler) ...) (key default key-handler) ...)
       (let ((method-str (symbol->string 'method)))
         (lambda (required ... #!key (tag #f) (key default) ...)
           (let ((required (required-handler required)) ... (key (key-handler key)) ...)
             (let ((arguments (filter identity `(,required ... ,key ...))))
               (rpc-call method-str #:arguments arguments #:tag tag))))))
      ((make-rpc-call method (key default key-handler) ...)
       (make-rpc-call (method) (key default key-handler) ...))))

  ;; Like `make-rpc-call` but defines the created procedure.
  (define-syntax define-rpc-call
    (syntax-rules ()
      ((define-rpc-call (method required ...) key ...)
       (define method (make-rpc-call (method required ...) key ...)))
      ((define-rpc-call method key ...)
       (define-rpc-call (method) key ...))))

  ;; Like `define-rpc-call` but exports the defined procedure.
  (define-syntax export-rpc-call
    (syntax-rules ()
      ((export-rpc-call (method required ...) key ...)
       (begin
         (export method)
         (define-rpc-call (method required ...) key ...)))
      ((export-rpc-call method key ...)
       (export-rpc-call (method) key ...))))

  ;; Export RPC procedures of sections 3.1 and 4.6. These procedures have a
  ;;   single optional parameter, `ids`. According to the RPC spec, no `ids`
  ;;   parameter means "all IDs". As a fail-safe, the default here is '(),
  ;;   which means "no IDs".
  ;; @see ids->argument
  (define-syntax export-3.1/4.6
    (syntax-rules ()
      ((export-3.1/4.6 method)
       (export-rpc-call method (ids '() ids->argument)))))

  (export-3.1/4.6 torrent-start)
  (export-3.1/4.6 torrent-start-now)
  (export-3.1/4.6 torrent-stop)
  (export-3.1/4.6 torrent-verify)
  (export-3.1/4.6 torrent-reannounce)

  (export-rpc-call
    torrent-set
    (bandwidth-priority    #f      (make-number->argument 'bandwidthPriority))
    (download-limit        #f      (make-number->argument 'downloadLimit))
    (download-limited      nothing (make-bool->argument   'downloadLimited))
    (files-wanted          #f      (make-array->argument  'files-wanted))
    (files-unwanted        #f      (make-array->argument  'files-unwanted))
    (honors-session-limits nothing (make-bool->argument   'honorsSessionLimits))
    (ids                   '()     ids->argument)
    (labels                #f      (make-array->argument  'labels))
    (location              #f      (make-string->argument 'location))
    (peer-limit            #f      (make-number->argument 'peer-limit))
    (priority-high         #f      (make-array->argument  'priority-high))
    (priority-low          #f      (make-array->argument  'priority-low))
    (priority-normal       #f      (make-array->argument  'priority-normal))
    (queue-position        #f      (make-number->argument 'queuePosition))
    (seed-idle-limit       #f      (make-number->argument 'seedIdleLimit))
    (seed-idle-mode        #f      (make-number->argument 'seedIdleMode))
    (seed-ratio-limit      #f      (make-number->argument 'seedRatioLimit))
    (seed-ratio-mode       #f      (make-number->argument 'seedRatioMode))
    (tracker-add           #f      (make-array->argument  'trackerAdd))
    (tracker-remove        #f      (make-array->argument  'trackerRemove))
    (tracker-replace       #f      (make-array->argument  'trackerReplace))
    (upload-limit          #f      (make-number->argument 'uploadLimit))
    (upload-limited        nothing (make-bool->argument   'uploadLimited)))

  (export-rpc-call (torrent-get (fields fields->argument)) (ids '() ids->argument) (format #f format->argument))

  ;; `source` must be a `filename` or `metainfo`, as described in section 3.4,
  ;;   and is constructed like so:
  ;;     (torrent-add (torrent-source/filename "/path/to/file.torrent") ...)
  ;;     (torrent-add (torrent-source/metainfo "<base64 torrent file>") ...)
  ;; Magnets go in `filename`.
  (export-rpc-call
    (torrent-add
      (source torrent-source->argument))
    (cookies            #f      (make-string->argument 'cookies))
    (download-dir       #f      (make-string->argument 'download-dir))
    (paused             nothing (make-bool->argument   'paused))
    (peer-limit         #f      (make-number->argument 'peer-limit))
    (bandwidth-priority #f      (make-number->argument 'bandwidthPriority))
    (files-wanted       #f      (make-array->argument  'files-wanted))
    (files-unwanted     #f      (make-array->argument  'files-unwanted))
    (priority-high      #f      (make-array->argument  'priority-high))
    (priority-low       #f      (make-array->argument  'priority-low))
    (priority-normal    #f      (make-array->argument  'priority-normal)))

  (export-rpc-call
    torrent-remove
    (ids               '()     ids->argument)
    (delete-local-data nothing (make-bool->argument 'delete-local-data)))

  (export-rpc-call
    (torrent-set-location
      (location (make-string->argument 'location)))
    (ids  '()     ids->argument)
    (move nothing (make-bool->argument 'move)))

  (export-rpc-call
    (torrent-rename-path
      (path (make-string->argument 'path))
      (name (make-string->argument 'name)))
    ; NOTE: This is a list of IDs, like other calls, but according to the spec
    ;       it must have exactly one ID. This is not enforced in the API.
    (ids '() ids->argument))

  (export-rpc-call
    session-set
    (alt-speed-down               #f      (make-number->argument 'alt-speed-down))
    (alt-speed-enabled            nothing (make-bool->argument   'alt-speed-enabled))
    (alt-speed-time-begin         #f      (make-number->argument 'alt-speed-time-begin))
    (alt-speed-time-day           #f      (make-number->argument 'alt-speed-time-day))
    (alt-speed-time-enabled       nothing (make-bool->argument   'alt-speed-time-enabled))
    (alt-speed-time-end           #f      (make-number->argument 'alt-speed-time-end))
    (alt-speed-up                 #f      (make-number->argument 'alt-speed-up))
    (blocklist-enabled            nothing (make-bool->argument   'blocklist-enabled))
    (blocklist-url                #f      (make-string->argument 'blocklist-url))
    (cache-size-mb                #f      (make-number->argument 'cache-size-mb))
    (dht-enabled                  nothing (make-bool->argument   'dht-enabled))
    (download-dir                 #f      (make-string->argument 'download-dir))
    (download-queue-enabled       nothing (make-bool->argument   'download-queue-enabled))
    (download-queue-size          #f      (make-number->argument 'download-queue-size))
    (encryption                   #f      (make-string->argument 'encryption))
    (idle-seeding-limit           #f      (make-number->argument 'idle-seeding-limit))
    (idle-seeding-limit-enabled   nothing (make-bool->argument   'idle-seeding-limit-enabled))
    (incomplete-dir               #f      (make-string->argument 'incomplete-dir))
    (incomplete-dir-enabled       nothing (make-bool->argument   'incomplete-dir-enabled))
    (lpd-enabled                  nothing (make-bool->argument   'lpd-enabled))
    (peer-limit-global            #f      (make-number->argument 'peer-limit-global))
    (peer-limit-per-torrent       #f      (make-number->argument 'peer-limit-per-torrent))
    (peer-port                    #f      (make-number->argument 'peer-port))
    (peer-port-random-on-start    nothing (make-bool->argument   'peer-port-random-on-start))
    (pex-enabled                  nothing (make-bool->argument   'pex-enabled))
    (port-forwarding-enabled      nothing (make-bool->argument   'port-forwarding-enabled))
    (queue-stalled-enabled        nothing (make-bool->argument   'queue-stalled-enabled))
    (queue-stalled-minutes        #f      (make-number->argument 'queue-stalled-minutes))
    (rename-partial-files         nothing (make-bool->argument   'rename-partial-files))
    (script-torrent-done-enabled  nothing (make-bool->argument   'script-torrent-done-enabled))
    (script-torrent-done-filename #f      (make-string->argument 'script-torrent-done-filename))
    (seed-queue-enabled           nothing (make-bool->argument   'seed-queue-enabled))
    (seed-queue-size              #f      (make-number->argument 'seed-queue-size))
    (seed-ratio-limit             #f      (make-number->argument 'seedRatioLimit))
    (seed-ratio-limited           nothing (make-bool->argument   'seedRatioLimited))
    (speed-limit-down             #f      (make-number->argument 'speed-limit-down))
    (speed-limit-down-enabled     nothing (make-bool->argument   'speed-limit-down-enabled))
    (speed-limit-up               #f      (make-number->argument 'speed-limit-up))
    (speed-limit-up-enabled       nothing (make-bool->argument   'speed-limit-up-enabled))
    (start-added-torrents         nothing (make-bool->argument   'start-added-torrents))
    (trash-original-torrent-files nothing (make-bool->argument   'trash-original-torrent-files))
    (units                        #f      (make-*->argument      'units proc-object))
    (utp-enabled                  nothing (make-bool->argument   'utp-enabled)))

  (export-rpc-call session-get (fields #f fields->argument))

  (export-rpc-call session-stats)
  (export-rpc-call blocklist-update)
  (export-rpc-call port-test)
  (export-rpc-call session-close)

  (export-3.1/4.6 queue-move-top)
  (export-3.1/4.6 queue-move-up)
  (export-3.1/4.6 queue-move-down)
  (export-3.1/4.6 queue-move-bottom)

  (export-rpc-call (free-space (path (make-string->argument 'path)))))
