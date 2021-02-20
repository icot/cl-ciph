(defpackage cl-ciph
  (:use :cl))
(in-package :cl-ciph)

(defun read-file (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (read in))))

(defun str->list (s)
  (loop for char across s collect char))

(defun get-mapping (str) nil)

(defun update-mapping (mapping key value) nil)

(defun apply-mapping (mapping str) nil)

(defun frequencies (str) nil)

(defun rot (mapping shift) nil)

(defun rot-brute-force (str) nil)
