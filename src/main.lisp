(defpackage cl-ciph
  (:use :cl)
;  (:import-from )
  (:export :main))
(in-package :cl-ciph)

(defun read-file (filename)
  "Returns FILENAME contents (text) as a string"
  (uiop:read-file-string filename))

(defun str->keyword-list (s)
  "Returns S a list of uppercase sensitive keywords"
  (loop for char across s collect (intern (string char) :keyword)))

(defun make-keys (keywords)
  "Removes duplicates and orders alphabetically the KEYWORDS list"
  (sort (remove-duplicates keywords :key #'symbol-name)
        (lambda (x y) (string< (symbol-name x) (symbol-name y)))))

(defun get-mapping-plist (str)
  "Returns a PLIST containing a keyword->string mapping of STR characters"
  (let ((ks (make-keys (str->keyword-list str))))
    (mapcan #'list ks (mapcar #'symbol-name ks))))
;; using mapcar we can generate an alist instead of a plist

(defmacro mpget (mapping k)
  "Returns MAPPING value for key K expressed as string"
  `(getf ,mapping (intern (string ,k) :keyword)))

(defmacro update-mapping (mapping key new-value)
  "Updates MAPPING KEY entry with NEW-VALUE"
  (let ((k (intern (string key) :keyword)))
    `(setf (getf ,mapping ,k) ,new-value)))

(defun apply-mapping (mapping str)
  "Applies a transformation based on MAPPING to STR"
  (format nil "~{~a~}"
          (loop for char across str collect (mpget mapping char))))

(defun mycount (str elem)
  "Counts appearances of ELEM in STR"
  (length (loop for c across str when (eql c elem) collect c)))

(defmacro symbol-char (symbol)
  "Returns a one char SYMBOL name as a character"
  `(char (symbol-name ,symbol) 0))

(defun frequencies (str)
  "Returns a PLIST containing the frequency distribution on STR"
  (let* ((keywords (str->keyword-list str))
         (ks (make-keys keywords))
         (l (length str)))
    (mapcan #'list ks (loop for k in ks collect (/ (mycount str (symbol-char k)) l)))))

;; TODO: Alphabet management
(defun ic (str)
  "Returns a PLIST containing the frequency distribution on STR"
  (let* ((keywords (str->keyword-list str))
         (ks (make-keys keywords))
         (l (length str))
         (c 26) ; English alphabet
         (denominator (* l (- l 1))))
    (/ (* c (apply #'+ (loop for k in ks collect (mycount str (symbol-char k))))) denominator)))

;; TODO: To utils
(defun circular (input)
  "Make a circular list from INPUT"
  (setf (cdr (last input)) input) input)

(defun shift (str &optional (shift 13))
  "Shift (Caesar's ) cipher of STR by SHIFT desplacements. case insensitive"
  (let* ((alph (loop for c across "abcdefghijklmnopqrstuvwxyz" collect c))
         (cipher (append alph alph))
         (ishift (if (< shift 0) (+ shift 26) shift))
         (crot (lambda (c) (let ((p (position c alph))))
                        (if p
                            (nth (+ p ishift) cipher)
                            c))))
    (format nil "~{~a~}" (loop for c across (string-downcase str) collect (funcall crot c)))))

(defun shift-brute-force (str)
  "Brute force shift STR, truncating to first 32 characters if needed. Returns ALIST ((shift . shifted str))"
  (if (> (length str) 32)
      (shift-brute-force (subseq str 0 31))
      (loop for s from 1 below 27 by 1 collect (cons s (shift str s)))))

;; TODO: add options
(defun sanitize-string (str)
  "Sanitize (remove non-letters) string"
  (remove-if (lambda (c) (str::digitp (string c)))
             (remove #\space
                     (str::upcase
                      (str::remove-punctuation
                       (str::collapse-whitespaces str))))))

;; TODO
(defun pattern-candidates (pattern)
  "Pattern valid words from *DICTIONARY*"
  nil)

;; TODO
(defun solve (str)
  "Substitution cipher solving REPL"
  nil)


;; TODO
(defun digraphs (str) nil)

;; Execution. Arg parsing + entry point
;; TODO Separate package?

(require 'unix-opts)

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
    (when it
      ,@body)))

(opts:define-opts
    (:name :help
    :description "This help"
    :short #\h
    :long "help")
    (:name :input
    :description "Path to file whose contents will be processed as input (e.g ciphertext)"
    :short #\i
      :long "input"
      :arg-parser #'identity
      :meta-var "FILE"))

(defun main (&rest argv)
  (declare (ignorable argv))

  (multiple-value-bind (options free-args)
      (handler-case
          (handler-bind ((opts:unknown-option #'unknown-option))
            (opts:get-opts argv))
            (opts:missing-arg (condition)
                              (format t "fatal: option ~s requires argument~%"
                                      (opts:option condition)))
            (opts:arg-parser-failed (condition)
                              (format t "fatal: Parser failer: ~s/~s~%"
                                      (opts:raw-arg condition)
                                      (opts:option condition)))
            (opts:missing-required-option (con)
                                          (format t "fatal: required option ~s~%" con)
                                          (opts:exit 1)))
            (when-option (options :help)
                        (opts:describe
                          :usage-of "cl-ciph"
                          :args     "[FREE-ARGS]"))
            (when-option (options :input)
                        (format t "Loading file ~s~%" (getf options :input)))
            (if free-args (format t "Unused arguments ~s~%" free-args)))
