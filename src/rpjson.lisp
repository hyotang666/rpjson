(in-package :cl-user)

(defpackage :rpjson
  (:use :cl)
  (:export))

(in-package :rpjson)

(defun |:-reader| (stream character)
  (declare (ignore stream character))
  (values))

(defun |,-reader| (stream character)
  (declare (ignore stream character))
  (values))

(defun |[-reader| (stream character)
  (declare (ignore character))
  (let ((contents (read-delimited-list #\] stream t)))
    (coerce contents 'vector)))

(defun |{-reader| (stream character)
  (declare (ignore character))
  (let ((contents (read-delimited-list #\} stream t))
        (object (make-hash-table :test #'eq))
        (*package* (find-package :keyword)))
    (loop :for (k v) :on contents :by #'cddr
          :do (setf (gethash
                      (read-from-string
                        (symbol-munger:camel-case->lisp-name k))
                      object)
                      v))
    object))

(defun |"-reader| (stream character)
  (let ((contents
         (let ((*readtable* (copy-readtable nil)))
           (funcall (get-macro-character character) stream character))))
    (cond ((string= "null" contents) 'null)
          ((string= "true" contents) t)
          ((string= "false" contents) nil)
          (t contents))))

(named-readtables:defreadtable rpjson
  (:macro-char #\: '|:-reader|)
  (:macro-char #\, '|,-reader|)
  (:macro-char #\[ '|[-reader|)
  (:macro-char #\{ '|{-reader|)
  (:macro-char #\" '|"-reader|))

(defun read-json (&optional stream errorp return)
  (let ((*readtable* (named-readtables:find-readtable 'rpjson)))
    (read stream errorp return)))