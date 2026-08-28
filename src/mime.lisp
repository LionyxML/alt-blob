(in-package #:alt.mime)

;;; Content types are decided by looking at the bytes, never by
;;; trusting the extension or the Content-Type the client
;;; sent. Anything that is not on this whitelist is refused, which is
;;; what keeps executables (ELF, Mach-O, PE) and SVG out of the store.
;;;
;;; Archives are on the list, and they are the one family that cannot
;;; be pinned down any further: a zip is a zip, and no amount of
;;; sniffing separates it from the jar, apk or docx that share its
;;; container. That is accepted here rather than worked around. An
;;; archive is stored and served as opaque bytes: never opened, never
;;; listed, never decompressed -- so there is no bomb to set off --
;;; and always handed over as an attachment, under the extension its
;;; magic bytes earned rather than the one it arrived with.

(defparameter *text-type* :text)

(defparameter *types*
  '((:text  "text/plain; charset=utf-8"    "txt"  "text"  t)
    (:png   "image/png"                    "png"  "png"   t)
    (:jpeg  "image/jpeg"                   "jpg"  "jpeg"  t)
    (:gif   "image/gif"                    "gif"  "gif"   t)
    (:webp  "image/webp"                   "webp" "webp"  t)
    (:pdf   "application/pdf"              "pdf"  "pdf"   t)
    (:zip   "application/zip"              "zip"  "zip"   nil)
    (:gzip  "application/gzip"             "gz"   "gzip"  nil)
    (:xz    "application/x-xz"             "xz"   "xz"    nil)
    (:bzip2 "application/x-bzip2"          "bz2"  "bzip2" nil)
    (:tar   "application/x-tar"            "tar"  "tar"   nil)
    (:rar   "application/vnd.rar"          "rar"  "rar"   nil)
    ;; Spelled out because the meta files are meant to be read with cat and a
    ;; keyword starting with a digit only prints as :|7Z|.
    (:sevenzip "application/x-7z-compressed" "7z" "7z" nil)
    (:zstd  "application/zstd"             "zst"  "zstd"  nil))
  "(keyword content-type extension label inline-p). INLINE-P decides
whether the blob is shown by the browser or handed over as a
download. Text and images are not active content and cannot be
re-interpreted as HTML behind X-Content-Type-Options: nosniff, and a
PDF -- the one inline entry that can carry script -- is confined by
the Content-Security-Policy sandbox that every blob response
carries. The archives are what the column is really for: there is
nothing to render, and nothing good comes of a browser trying, so they
always go out as attachments.")

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

(defun bzip2-p (bytes)
  "True for a bzip2 stream. The whole ten-byte signature is checked, not the
'BZh' prefix: those three bytes are printable ASCII, so a paste that opens with
\"BZhello\" would otherwise be filed as an archive. After the prefix comes the
block-size digit and then the BCD digits of pi that start the first block."
  (and (>= (length bytes) 10)
       (magic-p bytes '(#x42 #x5A #x68))                     ; BZh
       (<= (char-code #\1) (aref bytes 3) (char-code #\9))   ; block size
       (magic-p bytes '(#x31 #x41 #x59 #x26 #x53 #x59) 4)))  ; pi, in BCD

(defun tar-p (bytes)
  "True for a POSIX/GNU tar. The 'ustar' magic alone is too weak -- five bytes
landing at one fixed offset -- so the header checksum field is checked to be
octal as well. Pre-POSIX v7 tar carries no magic at all and is not accepted:
there would be nothing to recognize it by short of validating the checksum, and
guessing is how a store stops meaning anything."
  (and (>= (length bytes) 512)
       (magic-p bytes '(#x75 #x73 #x74 #x61 #x72) 257)       ; "ustar"
       (loop for i from 148 below 156
             for b = (aref bytes i)
             always (or (<= (char-code #\0) b (char-code #\7))
                        (= b (char-code #\Space))
                        (zerop b)))))

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
Empty input is refused: there is nothing worth a link in it. Text comes last,
so a signature that happens to be printable is matched as the archive it is
before the text path can claim it."
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
      ((or (magic-p bytes '(#x50 #x4B #x03 #x04))     ; PK\3\4, a local entry
           (magic-p bytes '(#x50 #x4B #x05 #x06))     ; PK\5\6, an empty zip
           (magic-p bytes '(#x50 #x4B #x07 #x08)))    ; PK\7\8, a spanned zip
       :zip)
      ((magic-p bytes '(#x1F #x8B)) :gzip)
      ((magic-p bytes '(#xFD #x37 #x7A #x58 #x5A #x00)) :xz)   ; \xfd7zXZ\0
      ((bzip2-p bytes) :bzip2)
      ((magic-p bytes '(#x52 #x61 #x72 #x21 #x1A #x07)) :rar)  ; Rar!\x1a\7, v4 and v5
      ((magic-p bytes '(#x37 #x7A #xBC #xAF #x27 #x1C)) :sevenzip)  ; 7z\xbc\xaf'\x1c
      ((magic-p bytes '(#x28 #xB5 #x2F #xFD)) :zstd)
      ((tar-p bytes) :tar)
      ((and (valid-utf8-p bytes) (printable-bytes-p bytes)) :text)
      (t nil))))
