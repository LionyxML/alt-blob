(in-package #:alt.store)

;;; On-disk layout, one directory per blob:
;;;
;;;   <data>/aaAsJk2/blob        raw bytes, exactly as uploaded
;;;   <data>/aaAsJk2/meta.lisp   (:id "aaAsJk2" :name "foo.pdf" :type :pdf
;;;                               :size 12345 :created 3958212345)
;;;
;;; Disk is the source of truth. A hash table mirrors the meta files so the
;;; listing can sort and paginate without touching the filesystem; it is rebuilt
;;; by scanning <data> at startup, so the store survives restarts (and can be
;;; rsync'd around) with no database in the picture.

(defvar *store-directory* nil
  "Pathname of the data directory, set by OPEN-STORE.")

(defvar *index* (make-hash-table :test #'equal)
  "Maps blob id (string) -> meta plist.")

(defvar *index-lock* (sb-thread:make-mutex :name "store-index")
  "Guards *INDEX* and id allocation (Hunchentoot serves one thread per request).")

(defun store-directory () *store-directory*)

(defun blob-dir (id)
  (merge-pathnames (make-pathname :directory (list :relative id))
                   *store-directory*))

(defun blob-path (id)
  (merge-pathnames "blob" (blob-dir id)))

(defun meta-path (id)
  ;; The extension is not decoration: RENAME-FILE merges the new name with the
  ;; old one, so renaming meta.tmp to a type-less "meta" would inherit .tmp
  ;; right back and the file would never land under its final name.
  (merge-pathnames "meta.lisp" (blob-dir id)))

(defun write-meta (id plist)
  "Write META atomically: a full file appears under a rename or not at all."
  (let ((tmp (merge-pathnames "meta.tmp" (blob-dir id))))
    (with-open-file (out tmp :direction :output :external-format :utf-8
                             :if-exists :supersede :if-does-not-exist :create)
      (with-standard-io-syntax
        ;; Readably-printed base strings come out as #A(...) literals; this file
        ;; is meant to be read by a person with `cat` as much as by READ.
        (let ((*package* (find-package :keyword))
              (*print-readably* nil)
              (*print-pretty* nil)
              (*print-case* :downcase))
          (prin1 plist out)))
      (terpri out))
    (rename-file tmp (meta-path id))))

(defun read-meta (id)
  "Read the meta plist for ID, or NIL when it is missing or unreadable.
*READ-EVAL* is off: the file is ours, but reading it should never evaluate."
  (handler-case
      (with-open-file (in (meta-path id) :direction :input :external-format :utf-8
                                         :if-does-not-exist nil)
        (when in
          (let* ((*read-eval* nil)
                 (form (read in nil nil)))
            (when (and (listp form) (stringp (getf form :id)))
              form))))
    (error () nil)))

(defun scan-store ()
  "Rebuild the index from disk. Directories without a readable meta file are
leftovers from an interrupted upload and get removed."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (dir (ignore-errors (uiop:subdirectories *store-directory*)))
      (let ((id (car (last (pathname-directory dir)))))
        (cond
          ((string= id ".tmp"))
          ((not (alt.utils:valid-id-p id)))
          (t (let ((meta (read-meta id)))
               (if meta
                   (setf (gethash id table) meta)
                   (ignore-errors
                     (uiop:delete-directory-tree dir :validate #'store-path-p))))))))
    (sb-thread:with-mutex (*index-lock*)
      (setf *index* table))
    (hash-table-count table)))

(defun store-path-p (path)
  "Validation gate for recursive deletes: PATH must live inside the store."
  (let ((store (namestring (truename *store-directory*)))
        (target (namestring path)))
    (and (> (length target) (length store))
         (string= store (subseq target 0 (length store))))))

(defun open-store (directory)
  "Point the store at DIRECTORY, creating it if needed, and load the index.
Returns the number of blobs found."
  (setf *store-directory* (uiop:ensure-directory-pathname
                           (uiop:ensure-absolute-pathname
                            directory (uiop:getcwd))))
  (ensure-directories-exist *store-directory*)
  (scan-store))

(defun fresh-id ()
  "Allocate an unused id and claim it by creating its directory. Called with
*INDEX-LOCK* held so two uploads cannot pick the same id."
  (loop for id = (alt.utils:random-base62 7)
        unless (or (gethash id *index*)
                   (probe-file (blob-dir id)))
          do (ensure-directories-exist (blob-dir id))
             (return id)))

(defun blob-name (filename type id)
  "Display name for a stored blob: the client's filename, sanitized and forced
to the extension the sniffed TYPE actually deserves. An .exe that turns out to
be text is stored as .txt, so nothing is ever named after what it is not.
An upload with no usable filename is named after its own id."
  (let* ((ext (alt.mime:type-extension type))
         (safe (alt.utils:safe-filename filename id))
         (dot (position #\. safe :from-end t))
         (base (if (and dot (plusp dot)) (subseq safe 0 dot) safe))
         (base (if (plusp (length base)) base id)))
    (format nil "~A.~A" base ext)))

(defun put-blob (bytes &key filename type)
  "Store BYTES under a fresh id and return its meta plist.
FILENAME is what the client called it, TYPE the keyword from ALT.MIME:SNIFF."
  (let ((id (sb-thread:with-mutex (*index-lock*) (fresh-id))))
    (handler-case
        (let ((meta (list :id id
                          :name (blob-name filename type id)
                          :type type
                          :size (length bytes)
                          :created (get-universal-time))))
          (with-open-file (out (blob-path id)
                               :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede
                               :if-does-not-exist :create)
            (write-sequence bytes out))
          (write-meta id meta)
          (sb-thread:with-mutex (*index-lock*)
            (setf (gethash id *index*) meta))
          meta)
      (error (e)
        ;; Leave nothing half-written behind.
        (ignore-errors (uiop:delete-directory-tree (blob-dir id)
                                                   :validate #'store-path-p))
        (error e)))))

(defun get-meta (id)
  (when (alt.utils:valid-id-p id)
    (sb-thread:with-mutex (*index-lock*)
      (gethash id *index*))))

(defun sort-key (meta key)
  (ecase key
    (:age (getf meta :created 0))
    (:size (getf meta :size 0))
    (:name (string-downcase (or (getf meta :name) "")))
    (:type (string-downcase (string (or (getf meta :type) ""))))
    (:id (getf meta :id ""))))

(defun list-blobs (&key (sort :age) (direction :desc) limit (offset 0))
  "Return (values page-of-metas total). SORT is :age, :size, :name, :type or
:id; DIRECTION is :asc or :desc."
  (let ((rows (sb-thread:with-mutex (*index-lock*)
                (loop for meta being the hash-values of *index*
                      collect meta))))
    (setf rows
          (sort rows
                (lambda (a b)
                  (let ((ka (sort-key a sort))
                        (kb (sort-key b sort)))
                    (if (stringp ka)
                        (string< ka kb)
                        (< ka kb))))))
    (when (eq direction :desc)
      (setf rows (nreverse rows)))
    (let* ((total (length rows))
           (offset (min (max 0 offset) total))
           (rows (nthcdr offset rows))
           (rows (if limit (subseq rows 0 (min limit (length rows))) rows)))
      (values rows total))))

(defun delete-blob (id)
  "Remove ID from disk and from the index. Returns T when something went away."
  (when (alt.utils:valid-id-p id)
    (let ((dir (blob-dir id)))
      (sb-thread:with-mutex (*index-lock*)
        (remhash id *index*))
      (when (probe-file dir)
        (ignore-errors
          (uiop:delete-directory-tree dir :validate #'store-path-p))
        t))))

(defun delete-blobs (ids)
  (count t (mapcar #'delete-blob ids)))

(defun count-blobs ()
  (sb-thread:with-mutex (*index-lock*) (hash-table-count *index*)))

(defun store-totals ()
  "Returns (values blob-count total-bytes)."
  (sb-thread:with-mutex (*index-lock*)
    (let ((count 0) (bytes 0))
      (loop for meta being the hash-values of *index*
            do (incf count)
               (incf bytes (or (getf meta :size) 0)))
      (values count bytes))))
