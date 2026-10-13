(in-package #:alt.web)

;; Defvars
(defvar *server* nil
  "Server instance (Hunchentoot acceptor).")

(defvar *base-url* "/"
  "Base URL used to build the link handed back after an upload.")

(defvar *admin-user* "admin"
  "Username required to access the admin-only routes.")

(defvar *admin-password* nil
  "Password required to access the admin-only routes.
When NIL or empty, admin routes are closed (fail-closed).")

(defvar *max-blob-bytes* (* 5 1024 1024)
  "Largest accepted upload, in bytes.")

(defvar *body-slack* 8192
  "Extra bytes allowed on top of *MAX-BLOB-BYTES* for multipart framing.")

(defvar *rate-limit-enabled* t
  "When true (default), public reads are rate limited per client IP.")

(defvar *rate-limit-max* 60
  "Max allowed reads per client IP within *RATE-LIMIT-WINDOW*.")

(defvar *rate-limit-window* 60
  "Length of the rate-limit sliding window, in seconds.")

(defvar *trust-proxy* nil
  "When true, take the client IP from the X-Forwarded-For header instead of the
socket peer. Enable ONLY when running behind a trusted reverse proxy (e.g. Caddy)
that sets the header; otherwise clients could spoof it to dodge the rate limit.")

(defvar *list-page-size* 50
  "Number of blobs shown per page on /admin.")

(defvar *rate-limit-table* (make-hash-table :test #'equal)
  "Maps client IP (string) -> list of request timestamps (universal-time).")

(defvar *rate-limit-lock* (sb-thread:make-mutex :name "rate-limit")
  "Guards *RATE-LIMIT-TABLE* (Hunchentoot serves one thread per request).")

;; Parameters
(defparameter *www-directory*
  (asdf:system-relative-pathname :alt "www/"))

(defparameter *template-directory*
  (asdf:system-relative-pathname :alt "templates/"))

(defparameter *template-files*
  '("index.html" "about.html" "new.html" "admin.html" "confirm-delete.html"
    "not-found.html" "rate-limited.html" "unauthorized.html" "error.html")
  "Templates rendered by routes; precompiled at startup. (layout.html is the
base and gets pulled in when its children compile.)")

;; Helpers
(defvar *template-cache* (make-hash-table :test #'equal)
  "Maps template name (string) -> compiled djula template.
Compiling on every request races under Hunchentoot's thread-per-request model
and is slow; compile once, reuse.")

(defvar *template-cache-lock* (sb-thread:make-mutex :name "template-cache"))

(defun compiled-template (template-name)
  "Return the compiled template for TEMPLATE-NAME, compiling and caching on
first use. Guarded so concurrent first hits compile only once."
  (or (gethash template-name *template-cache*)
      (sb-thread:with-mutex (*template-cache-lock*)
        (or (gethash template-name *template-cache*)
            (setf (gethash template-name *template-cache*)
                  (djula:compile-template*
                   (merge-pathnames template-name *template-directory*)))))))

(defun admin-authorized-p ()
  "True when the request carries valid admin Basic-Auth credentials.
Fail-closed: if no admin password is configured, always returns NIL."
  (and (stringp *admin-password*)
       (plusp (length *admin-password*))
       (ignore-errors
        (multiple-value-bind (user password) (hunchentoot:authorization)
          (and (alt.utils:constant-time-string= user *admin-user*)
               (alt.utils:constant-time-string= password *admin-password*))))))

(defun render (template-name &rest args)
  (apply
   #'djula:render-template*
   (compiled-template template-name)
   nil
   :title (getf alt:*app* :name)
   :description (getf alt:*app* :description)
   :version (getf alt:*app* :version)
   :repository (getf alt:*app* :repository)
   :base-url *base-url*
   :max-size (alt.utils:human-size *max-blob-bytes*)
   :types (format nil "~{~A~^, ~}" (alt.mime:accepted-types))
   args))

(defun wants-html-p ()
  "True for browsers, false for curl and friends. An explicit `raw` parameter
forces the plain-text answer either way."
  (and (null (hunchentoot:get-parameter "raw"))
       (let ((accept (hunchentoot:header-in* :accept)))
         (and (stringp accept)
              (search "text/html" accept)
              t))))

(defun plain (status text)
  "Answer STATUS with a plain-text body."
  (setf (hunchentoot:return-code*) status
        (hunchentoot:content-type*) "text/plain; charset=utf-8")
  (format nil "~A~%" text))

(defun not-found ()
  (if (wants-html-p)
      (progn (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
             (render "not-found.html"))
      (plain hunchentoot:+http-not-found+ "Not found.")))

(defun blob-url (id)
  (format nil "~Ab/~A" *base-url* id))

;;; Rate limiting (public reads only; writes are behind admin auth)
(defun client-ip ()
  "The IP used for rate limiting. By default the real socket peer
(hunchentoot:remote-addr*), which cannot be spoofed. When *TRUST-PROXY* is on,
use hunchentoot:real-remote-addr, which reads X-Forwarded-For — correct only
behind a trusted reverse proxy (otherwise the client controls the header)."
  (if *trust-proxy*
      (hunchentoot:real-remote-addr)
      (hunchentoot:remote-addr*)))

(defun rate-limit-check (ip)
  "Record a request from IP and decide whether it is allowed.
Sliding window: keep only timestamps within the last *RATE-LIMIT-WINDOW*
seconds; allow (and record) while fewer than *RATE-LIMIT-MAX* remain.
Returns (values allowed-p retry-after-seconds)."
  (let ((now (get-universal-time))
        (window *rate-limit-window*)
        (max *rate-limit-max*))
    (sb-thread:with-mutex (*rate-limit-lock*)
      (let ((recent (remove-if (lambda (ts) (< ts (- now window)))
                               (gethash ip *rate-limit-table*))))
        (cond
          ((< (length recent) max)
           (setf (gethash ip *rate-limit-table*) (cons now recent))
           (values t 0))
          (t
           ;; Denied. Drop the recorded (but unused) entry back so a rejected
           ;; request does not extend the window, and report when the oldest
           ;; in-window stamp ages out.
           (setf (gethash ip *rate-limit-table*) recent)
           (values nil (max 1 (- (+ (first (last recent)) window) now)))))))))

;;; Decorators
(defun @require-admin (next)
  "easy-routes decorator: gate a route behind admin Basic Auth.
Calls NEXT when authorized, otherwise responds 401 with WWW-Authenticate."
  (if (admin-authorized-p)
      (funcall next)
      (hunchentoot:require-authorization "AltBlob admin")))

(defun same-origin-request-p ()
  "Basic Auth credentials ride along on every browser request, so a POST needs
its own cross-site check. Sec-Fetch-Site settles it on modern browsers; older
ones are judged by Origin against Host. A request with neither header did not
come from a browser (curl, w3m) and cannot be a CSRF vector, so it passes."
  (let ((fetch-site (hunchentoot:header-in* :sec-fetch-site))
        (origin (hunchentoot:header-in* :origin))
        (host (hunchentoot:header-in* :host)))
    (cond
      ((stringp fetch-site)
       (and (member fetch-site '("same-origin" "none") :test #'string-equal) t))
      ((and (stringp origin) (stringp host))
       (let ((authority (ignore-errors (quri:uri-authority (quri:uri origin)))))
         (and (stringp authority) (string-equal authority host))))
      ((stringp origin) nil)
      (t t))))

(defun @same-origin (next)
  "easy-routes decorator: refuse cross-site writes."
  (if (same-origin-request-p)
      (funcall next)
      (plain hunchentoot:+http-forbidden+ "Cross-site request refused.")))

(defun @limit-body (next)
  "easy-routes decorator: refuse oversized bodies before reading them.
A body with no Content-Length is refused too — without it there is no cap to
enforce, and every client we care about sends one."
  (let* ((raw (hunchentoot:header-in* :content-length))
         (len (and raw (parse-integer raw :junk-allowed t))))
    (cond
      ((null len)
       (plain 411 "Content-Length required."))
      ((> len (+ *max-blob-bytes* *body-slack*))
       (plain 413 (format nil "Too large. Limit is ~A."
                          (alt.utils:human-size *max-blob-bytes*))))
      (t (funcall next)))))

(defun @rate-limit (next)
  "easy-routes decorator: per-IP rate limit on public reads. See CLIENT-IP for
how the client address is resolved (socket peer, or X-Forwarded-For behind a
trusted proxy when --trust-proxy / TRUST_PROXY is set)."
  (if *rate-limit-enabled*
      (multiple-value-bind (allowed retry-after) (rate-limit-check (client-ip))
        (if allowed
            (funcall next)
            (progn
              (setf (hunchentoot:header-out :retry-after) retry-after)
              (if (wants-html-p)
                  (progn (setf (hunchentoot:return-code*) 429)
                         (render "rate-limited.html" :retry retry-after))
                  (plain 429 (format nil "Slow down. Retry in ~A seconds."
                                     retry-after))))))
      (funcall next)))

;;; Uploads
(defun file-bytes (path &optional (limit *max-blob-bytes*))
  "Read PATH into an octet vector, or NIL when it is larger than LIMIT."
  (with-open-file (in path :element-type '(unsigned-byte 8)
                           :if-does-not-exist nil)
    (when in
      (let ((size (file-length in)))
        (when (<= size limit)
          (let ((buf (make-array size :element-type '(unsigned-byte 8))))
            (read-sequence buf in)
            buf))))))

(defun text-bytes (string)
  "UTF-8 octets for a pasted string, with CRLF line endings normalized to LF
(browsers send CRLF from a textarea; nobody wants that in the stored file)."
  (let ((clean (remove #\Return string)))
    (sb-ext:string-to-octets clean :external-format :utf-8)))

(defun multipart-request-p ()
  (let ((ct (hunchentoot:header-in* :content-type)))
    (and (stringp ct) (search "multipart/form-data" ct) t)))

(defun multipart-upload ()
  "Pull the upload out of a multipart form: the file field wins, the textarea
is the fallback. Returns (values bytes filename) or (values nil reason)."
  (let* ((raw-file (or (hunchentoot:post-parameter "f")
                       (hunchentoot:post-parameter "file")))
         ;; A form submitted with the file input left empty still sends the
         ;; part, with an empty filename. That is not an upload.
         (file (when (and (listp raw-file)
                          (car raw-file)
                          (stringp (second raw-file))
                          (plusp (length (second raw-file))))
                 raw-file))
         (text (hunchentoot:post-parameter "text")))
    (cond
      (file
       (destructuring-bind (tmp-path filename &optional content-type) file
         (declare (ignore content-type))
         (let ((bytes (file-bytes tmp-path)))
           (cond
             ((null bytes)
              (values nil (format nil "Too large. Limit is ~A."
                                  (alt.utils:human-size *max-blob-bytes*))))
             ((zerop (length bytes)) (values nil "Empty upload."))
             (t (values bytes filename))))))
      ((and (stringp text) (plusp (length (string-trim '(#\Space #\Newline #\Return #\Tab) text))))
       (values (text-bytes text) (hunchentoot:post-parameter "name")))
      (t (values nil "Nothing to upload: pick a file or paste some text.")))))

(defun raw-upload ()
  "Read a plain request body (curl --data-binary). Returns (values bytes name)
or (values nil reason)."
  (let ((bytes (hunchentoot:raw-post-data :force-binary t)))
    (cond
      ((or (null bytes) (zerop (length bytes)))
       (values nil "Empty upload."))
      ((> (length bytes) *max-blob-bytes*)
       (values nil (format nil "Too large. Limit is ~A."
                           (alt.utils:human-size *max-blob-bytes*))))
      (t (values bytes (hunchentoot:get-parameter "name"))))))

;;; Routes
(easy-routes:defroute home ("/") ()
  (render "index.html"))

(easy-routes:defroute about-page ("/about") ()
  (render "about.html"))

(defun serve-blob (id)
  "Answer with the stored bytes, or 404. Accepted types are shown by the browser
rather than downloaded; ?dl=1 forces the download. Every response carries the
headers that keep a blob from being treated as active content."
  (let ((meta (alt.store:get-meta id)))
    (if (null meta)
        (not-found)
        (let* ((type (getf meta :type))
               (path (alt.store:blob-path id)))
          (if (not (probe-file path))
              (not-found)
              (progn
                (setf (hunchentoot:header-out :content-security-policy)
                      "default-src 'none'; sandbox"
                      (hunchentoot:header-out :x-robots-tag) "noindex, nofollow"
                      (hunchentoot:header-out :content-disposition)
                      (format nil "~A; filename=\"~A\""
                              (if (and (alt.mime:type-inline-p type)
                                       (null (hunchentoot:get-parameter "dl")))
                                  "inline"
                                  "attachment")
                              (getf meta :name)))
                (hunchentoot:handle-static-file
                 path (alt.mime:type-content-type type))))))))

(easy-routes:defroute fetch-blob ("/b/:id" :decorators (@rate-limit))
    (&path (id 'string))
  (serve-blob id))

(easy-routes:defroute head-blob ("/b/:id" :method :head :decorators (@rate-limit))
    (&path (id 'string))
  (serve-blob id))

(easy-routes:defroute new-page ("/new" :method :get :decorators (@require-admin)) ()
  (render "new.html"))

(easy-routes:defroute new-submit
    ("/new" :method :post :decorators (@limit-body @require-admin @same-origin)) ()
  (multiple-value-bind (bytes filename)
      (if (multipart-request-p) (multipart-upload) (raw-upload))
    (flet ((fail (message)
             (if (wants-html-p)
                 (progn (setf (hunchentoot:return-code*) 400)
                        (render "new.html" :error message))
                 (plain 400 message))))
      (cond
        ((null bytes) (fail filename))
        (t
         (let ((type (alt.mime:sniff bytes)))
           (if (null type)
               (fail (format nil "Refused: not an accepted type. Allowed: ~{~A~^, ~}."
                             (alt.mime:accepted-types)))
               (let* ((meta (alt.store:put-blob bytes :type type
                                                      :filename filename))
                      (id (getf meta :id))
                      (url (blob-url id)))
                 (if (wants-html-p)
                     (render "new.html" :created url :created-id id
                                        :created-name (getf meta :name)
                                        :created-size (alt.utils:human-size
                                                       (getf meta :size)))
                     (plain 201 url))))))))))

(defun parse-sort (raw)
  (cond ((null raw) :age)
        ((string-equal raw "size") :size)
        ((string-equal raw "name") :name)
        ((string-equal raw "type") :type)
        ((string-equal raw "id") :id)
        (t :age)))

(defun parse-direction (raw sort)
  (cond ((and (stringp raw) (string-equal raw "asc")) :asc)
        ((and (stringp raw) (string-equal raw "desc")) :desc)
        ;; Newest and biggest first are the useful defaults; names read better
        ;; the other way round.
        ((member sort '(:name :type :id)) :asc)
        (t :desc)))

(defun row (meta)
  "Meta plist turned into what the template reads. Keys are interned lowercase
so Djula's dotted lookup (row.human_size) finds them."
  (let ((type (getf meta :type)))
    (list :|id| (getf meta :id)
          :|name| (getf meta :name)
          :|type| (alt.mime:type-label type)
          :|size| (getf meta :size)
          :|human_size| (alt.utils:human-size (getf meta :size))
          :|age| (alt.utils:human-age (getf meta :created))
          :|created| (alt.utils:universal-to-iso8601 (getf meta :created)))))

(defun other-direction (direction)
  (if (eq direction :asc) "desc" "asc"))

(defparameter *columns*
  '((:id . "id") (:name . "name") (:type . "type") (:size . "size") (:age . "age"))
  "Listing columns, in the order the template lays them out. Every one of them
sorts, which is the whole navigation story: no search box, no filters.")

(defun sort-columns (sort direction)
  "Header cells: clicking a column sorts by it, clicking the active one flips."
  (loop for (key . label) in *columns*
        collect (let* ((active (eq key sort))
                       (next (if active
                                 (other-direction direction)
                                 (if (member key '(:name :type :id)) "asc" "desc"))))
                  (list :|label| label
                        :|url| (format nil "/admin?sort=~A&dir=~A" label next)
                        :|active| active
                        ;; Everything but the name is a short field, centered in
                        ;; its cell; the header follows so the column reads straight.
                        :|center| (not (eq key :name))
                        :|arrow| (if (eq direction :asc) "^" "v")))))

(defun page-url (sort direction page)
  (format nil "/admin?sort=~A&dir=~A&page=~D"
          (string-downcase (symbol-name sort))
          (string-downcase (symbol-name direction))
          page))

(easy-routes:defroute list-blobs-page ("/admin" :decorators (@require-admin)) ()
  (let* ((sort (parse-sort (hunchentoot:parameter "sort")))
         (direction (parse-direction (hunchentoot:parameter "dir") sort))
         (raw (hunchentoot:parameter "raw")))
    (if raw
        ;; TSV for the shell: id, name, type, size, created.
        (multiple-value-bind (rows total)
            (alt.store:list-blobs :sort sort :direction direction)
          (declare (ignore total))
          (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
          (with-output-to-string (out)
            (dolist (meta rows)
              (format out "~A~C~A~C~A~C~D~C~A~%"
                      (getf meta :id) #\Tab
                      (getf meta :name) #\Tab
                      (alt.mime:type-label (getf meta :type)) #\Tab
                      (getf meta :size) #\Tab
                      (alt.utils:universal-to-iso8601 (getf meta :created))))))
        (let* ((size *list-page-size*)
               (total (alt.store:count-blobs))
               (pages (max 1 (ceiling total size)))
               (page (min pages (max 1 (or (parse-integer
                                            (or (hunchentoot:parameter "page") "1")
                                            :junk-allowed t)
                                           1))))
               (offset (* (1- page) size)))
          (multiple-value-bind (rows total)
              (alt.store:list-blobs :sort sort :direction direction
                                    :limit size :offset offset)
            (multiple-value-bind (count bytes) (alt.store:store-totals)
              (declare (ignore count))
              (render "admin.html"
                      :blobs (mapcar #'row rows)
                      :columns (sort-columns sort direction)
                      :total total
                      :disk (alt.utils:human-size bytes)
                      :page page :pages pages
                      :from (if (zerop (length rows)) 0 (1+ offset))
                      :to (+ offset (length rows))
                      :prev-url (when (> page 1) (page-url sort direction (1- page)))
                      :next-url (when (< page pages) (page-url sort direction (1+ page))))))))))

(defun posted-ids ()
  "Every `id` field of the POST, kept only when it names a blob we still have."
  (remove nil
          (mapcar (lambda (pair)
                    (when (string= (car pair) "id")
                      (let ((id (cdr pair)))
                        (when (and (stringp id) (alt.store:get-meta id))
                          id))))
                  (hunchentoot:post-parameters*))))

(easy-routes:defroute delete-blobs-route
    ("/admin/delete" :method :post :decorators (@limit-body @require-admin @same-origin)) ()
  (let ((ids (posted-ids))
        (confirmed (hunchentoot:post-parameter "confirm")))
    (cond
      ((null ids)
       (if (wants-html-p)
           (hunchentoot:redirect "/admin")
           (plain 400 "No existing ids given.")))
      (confirmed
       (let ((n (alt.store:delete-blobs ids)))
         (if (wants-html-p)
             (hunchentoot:redirect "/admin")
             (plain 200 (format nil "Deleted ~D." n)))))
      ((wants-html-p)
       ;; No confirmation yet: show what is about to go, and let the user post
       ;; it back with confirm=1.
       (render "confirm-delete.html"
               :blobs (mapcar (lambda (id) (row (alt.store:get-meta id))) ids)
               :count (length ids)))
      (t
       (plain 409 (format nil "Would delete ~D: ~{~A~^ ~}. Repeat with confirm=1."
                          (length ids) ids))))))

;;; Server
(defclass alt-acceptor (easy-routes:easy-routes-acceptor) ()
  (:documentation "Routing acceptor that serves our own templates for the
status pages Hunchentoot would otherwise generate itself (404, 401, ...)."))

(defmethod hunchentoot:acceptor-dispatch-request :before
    ((acceptor alt-acceptor) request)
  "Defaults for every response; handlers may override them (blobs set a
stricter CSP)."
  (declare (ignore request))
  (setf (hunchentoot:header-out :x-content-type-options) "nosniff"
        (hunchentoot:header-out :referrer-policy) "strict-origin-when-cross-origin"
        (hunchentoot:header-out :content-security-policy) "frame-ancestors 'none'"))

(defmethod hunchentoot:acceptor-status-message
    ((acceptor alt-acceptor) status-code &key &allow-other-keys)
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (case status-code
    (404 (render "not-found.html"))
    (401 (render "unauthorized.html"))
    (t (render "error.html"
               :status status-code
               :reason (hunchentoot:reason-phrase status-code)))))

(defun start-server (&key port base-url admin-user admin-password
                       max-blob (rate-limit-enabled t)
                       rate-limit-max rate-limit-window
                       trust-proxy max-threads accept-backlog list-page-size)
  (flet ((as-int (v default)
           (etypecase v
             (null default)
             (integer v)
             (string (or (parse-integer v :junk-allowed t) default)))))
    (let ((port (as-int port 3900))
          (max-threads (as-int max-threads 100))
          (accept-backlog (as-int accept-backlog 200)))

      (setf *base-url* (if (and (stringp base-url)
                                (plusp (length base-url))
                                (char= (char base-url (1- (length base-url))) #\/))
                           base-url
                           (format nil "~A/" (or base-url ""))))
      (when admin-user
        (setf *admin-user* admin-user))
      (setf *admin-password* admin-password)
      (setf *max-blob-bytes* (max 1024 (as-int max-blob (* 5 1024 1024))))
      (setf *rate-limit-enabled* rate-limit-enabled)
      (setf *rate-limit-max* (max 1 (as-int rate-limit-max 60)))
      (setf *rate-limit-window* (max 1 (as-int rate-limit-window 60)))
      (setf *trust-proxy* trust-proxy)
      (setf *list-page-size* (max 1 (as-int list-page-size 50)))

      (format t "~&Starting the web server on port ~a~&" port)
      (force-output)

      (pushnew
       (hunchentoot:create-folder-dispatcher-and-handler "/" *www-directory*)
       hunchentoot:*dispatch-table*
       :test #'equal)

      ;; Precompile templates now (single-threaded) so no request thread hits a
      ;; cold cache and races the first compile.
      (dolist (tpl *template-files*)
        (compiled-template tpl))

      (setf *server*
            (make-instance 'alt-acceptor
                           :port port
                           :taskmaster
                           (make-instance 'hunchentoot:one-thread-per-connection-taskmaster
                                          :max-thread-count max-threads
                                          :max-accept-count (+ max-threads accept-backlog))))

      (hunchentoot:start *server*))))

(defun stop-server ()
  (hunchentoot:stop *server*))
