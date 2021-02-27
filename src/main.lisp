(defpackage cl-ciph
  (:use :cl))
(in-package :cl-ciph)

(defun read-file (filename)
  "Returns FILENAME contents (text) as a string"
  (with-open-file (in filename)
    (with-standard-io-syntax
      (read in))))

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
  "Returns MAPPING value for key K expresses as string"
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
    (mapcan #'list ks (loop for k in ks collect (float (/ (mycount str (symbol-char k)) l))))))

(defun rot (str shift) nil)

(defun rot-brute-force (str) nil)
