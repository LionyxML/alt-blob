(in-package #:alt.mime)

;;; Content types are decided by looking at the bytes, never by trusting the
;;; extension or the Content-Type the client sent. Anything that is not on this
;;; whitelist is refused, which is what keeps executables (ELF, Mach-O, PE),
;;; archives (zip, and therefore jar/apk/docx) and SVG out of the store.

(defparameter *text-type* :text)

(defparameter *types*
  '((:text  "text/plain; charset=utf-8" "txt"  "text"  t)
    (:png   "image/png"                 "png"  "png"   t)
    (:jpeg  "image/jpeg"                "jpg"  "jpeg"  t)
    (:gif   "image/gif"                 "gif"  "gif"   t)
    (:webp  "image/webp"                "webp" "webp"  t)
    (:pdf   "application/pdf"           "pdf"  "pdf"   t))
  "(keyword content-type extension label inline-p). INLINE-P decides whether the
blob is shown by the browser or handed over as a download. Everything on this
list is safe to show: text and images are not active content and cannot be
re-interpreted as HTML behind X-Content-Type-Options: nosniff, and a PDF -- the
one entry that can carry script -- is confined by the Content-Security-Policy
sandbox that every blob response carries. The column stays because it is the
switch a less trustworthy type would need.")

(defun entry (type) (assoc type *types*))

(defun type-content-type (type) (second (entry type)))
(defun type-extension (type) (third (entry type)))
(defun type-label (type) (fourth (entry type)))
(defun type-inline-p (type) (fifth (entry type)))

(defun accepted-types ()
  "Human-readable list of what may be uploaded, for the help text and forms."
  (mapcar #'fourth *types*))

(defun magic-p (bytes signature &optional (offset 0))
  "True when BYTES carries SIGNATURE (a list of octets) at OFFSET."
  (and (>= (length bytes) (+ offset (length signature)))
       (loop for b in signature
             for i from offset
             always (= b (aref bytes i)))))

(defun valid-utf8-p (bytes)
  "Strict UTF-8 check: rejects overlong encodings, surrogates and anything
above U+10FFFF, so binary junk cannot sneak in through the text path."
  (let ((i 0)
        (n (length bytes)))
    (loop while (< i n)
          do (let* ((b (aref bytes i))
                    (len (cond ((< b #x80) 1)
                               ((<= #xC2 b #xDF) 2)
                               ((<= #xE0 b #xEF) 3)
                               ((<= #xF0 b #xF4) 4)
                               (t (return-from valid-utf8-p nil)))))
               (when (> (+ i len) n)
                 (return-from valid-utf8-p nil))
               (loop for k from 1 below len
                     for c = (aref bytes (+ i k))
                     unless (<= #x80 c #xBF)
                       do (return-from valid-utf8-p nil))
               (let ((c (if (> len 1) (aref bytes (1+ i)) 0)))
                 (case len
                   (3 (when (or (and (= b #xE0) (< c #xA0))    ; overlong
                                (and (= b #xED) (>= c #xA0)))  ; surrogate
                        (return-from valid-utf8-p nil)))
                   (4 (when (or (and (= b #xF0) (< c #x90))    ; overlong
                                (and (= b #xF4) (>= c #x90)))  ; > U+10FFFF
                        (return-from valid-utf8-p nil)))))
               (incf i len)))
    t))

(defun printable-bytes-p (bytes)
  "Reject control characters other than tab, newline and carriage return.
A NUL or an escape sequence means we are not looking at a text paste."
  (every (lambda (b)
           (and (/= b #x7f)
                (or (>= b #x20) (member b '(9 10 13)))))
         bytes))

(defun sniff (bytes)
  "Return the keyword for BYTES, or NIL when the content is not accepted.
Empty input is refused: there is nothing worth a link in it."
  (when (and (typep bytes '(vector (unsigned-byte 8)))
             (plusp (length bytes)))
    (cond
      ((magic-p bytes '(#x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A)) :png)
      ((magic-p bytes '(#xFF #xD8 #xFF)) :jpeg)
      ((or (magic-p bytes '(71 73 70 56 55 97))       ; GIF87a
           (magic-p bytes '(71 73 70 56 57 97)))      ; GIF89a
       :gif)
      ((and (magic-p bytes '(82 73 70 70))            ; RIFF
            (magic-p bytes '(87 69 66 80) 8))         ; WEBP
       :webp)
      ((magic-p bytes '(37 80 68 70 45)) :pdf)        ; %PDF-
      ((and (valid-utf8-p bytes) (printable-bytes-p bytes)) :text)
      (t nil))))
