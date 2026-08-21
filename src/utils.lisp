(in-package #:alt.utils)

(defparameter *base62-alphabet*
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(defun random-base62 (&optional (length 7))
  "Generate an unpredictable LENGTH-char base62 string from OS entropy.
Reads bytes from /dev/urandom; rejection-samples to avoid modulo bias
(256 is not a multiple of 62, so bytes >= 248 are discarded)."
  (with-open-file (urandom "/dev/urandom" :element-type '(unsigned-byte 8))
    (let ((out (make-string length)))
      (dotimes (i length out)
        (let ((byte (read-byte urandom)))
          (loop while (>= byte 248)
                do (setf byte (read-byte urandom)))
          (setf (char out i)
                (char *base62-alphabet* (mod byte 62))))))))

(defun valid-id-p (id)
  "True when ID is a plausible blob id: 1-16 base62 characters, nothing else.
Every path built from user input is gated on this, so `..` and `/` never
reach the filesystem."
  (and (stringp id)
       (<= 1 (length id) 16)
       (every (lambda (c) (find c *base62-alphabet*)) id)))

(defun human-size (bytes)
  "Compact size for the listing: 934 B, 12.3 KB, 4.7 MB."
  (let ((b (or bytes 0)))
    (cond
      ((< b 1024) (format nil "~D B" b))
      ((< b (* 1024 1024)) (format nil "~,1F KB" (/ b 1024.0)))
      (t (format nil "~,1F MB" (/ b (* 1024.0 1024.0)))))))

(defun human-age (universal-time)
  "Rough age of UNIVERSAL-TIME as a short string: 4m, 3h, 12d."
  (let ((secs (max 0 (- (get-universal-time) (or universal-time 0)))))
    (cond
      ((< secs 60) (format nil "~Ds" secs))
      ((< secs 3600) (format nil "~Dm" (floor secs 60)))
      ((< secs 86400) (format nil "~Dh" (floor secs 3600)))
      (t (format nil "~Dd" (floor secs 86400))))))

(defun universal-to-iso8601 (universal-time)
  "UTC timestamp, used for the title attribute and the TSV listing."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (or universal-time 0) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour min sec)))

(defun safe-filename (name fallback)
  "Sanitize a client-supplied filename for use in Content-Disposition.
Keeps a conservative character set, drops any directory part, caps the
length, and falls back to FALLBACK when nothing usable remains."
  (let* ((name (if (stringp name) name ""))
         (base (subseq name (1+ (or (position-if (lambda (c) (member c '(#\/ #\\)))
                                                 name :from-end t)
                                    -1))))
         (clean (remove-if-not
                 (lambda (c)
                   (or (alphanumericp c) (find c "._- ")))
                 base))
         (clean (string-trim " ." clean)))
    (if (plusp (length clean))
        (subseq clean 0 (min (length clean) 96))
        fallback)))

(defun constant-time-string= (a b)
  "Compare two strings in time independent of where they first differ.
Reduces timing-oracle leakage on credential checks. Returns NIL unless
both are strings of equal length with identical contents."
  (and (stringp a) (stringp b)
       (= (length a) (length b))
       (loop with diff = 0
             for ca across a
             for cb across b
             do (setf diff (logior diff (logxor (char-code ca) (char-code cb))))
             finally (return (zerop diff)))))
