(defpackage cl-ciph
  (:use :cl))
(in-package :cl-ciph)

(defun read-file (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (read in))))

(defparameter s "hola esto es un test")

(defun str->keyword-list (s)
  (loop for char across s collect (intern (string char) :keyword)))

(defparameter a (str->keyword-list "hola esto es un test"))

(defun get-mapping-plist (str)
  (let ((ks (sort (remove-duplicates (str->keyword-list str) :key #'symbol-name)
                  (lambda (x y) (string< (symbol-name x) (symbol-name y))))))
    (mapcan #'list ks (mapcar #'symbol-name ks))))
;; using mapcar we can generate an alist instead of a plist

(defmacro mpget (mapping k)
  `(getf ,mapping (intern (string ,k) :keyword)))

(defparameter mp (get-mapping-plist s))

(defmacro update-mapping (mapping key new-value)
  (let ((k (intern (string key) :keyword)))
    `(setf (getf ,mapping ,k) ,new-value)))

(defun apply-mapping (mapping str) nil)

(defun frequencies (str) nil)
(defun rot (mapping shift) nil)
(defun rot-brute-force (str) nil)
