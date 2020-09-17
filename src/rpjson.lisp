(in-package :cl-user)

(defpackage :rpjson
  (:use :cl)
  (:export))

(in-package :rpjson)

(defun |:-reader| (stream character)
  (declare (ignore stream character))
  (values))

(defun |,-reader| (stream character)
  (declare (ignore character))
  (case (peek-char t stream)
    ((#\] #\}) (values))
    ((#\,) nil)
    (otherwise (read stream t t t))))

(defun |[-reader| (stream character)
  (declare (ignore character))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\, '|,-reader|)
    (set-macro-character #\" '|"-reader|)
    (if (char= #\, (peek-char t stream))
        `(vector nil ,@(read-delimited-list #\] stream t))
        `(vector ,@(read-delimited-list #\] stream t)))))

(defun |{-reader| (stream character)
  (declare (ignore character))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\: '|:-reader|)
    (set-macro-character #\, '|,-reader|)
    (set-macro-character #\" '|"-reader|)
    (let ((contents (read-delimited-list #\} stream t))
          (var (gensym "HASH-TABLE"))
          (package (find-package :keyword)))
      `(let ((,var (make-hash-table :test #'eq)))
         ,@(loop :for (k v) :on contents :by #'cddr
                 :collect `(setf (gethash ,(intern k package) ,var) ,v))
         ,var))))

(let ((reader (get-macro-character #\" (copy-readtable nil))))
  (defun |"-reader| (stream character)
    (let ((contents (funcall reader stream character)))
      (cond ((string= "null" contents) ''null)
            ((string= "true" contents) t)
            ((string= "false" contents) nil)
            (t contents)))))

(named-readtables:defreadtable rpjson
  (:macro-char #\[ '|[-reader|)
  (:macro-char #\] (get-macro-character #\) (copy-readtable nil)))
  (:macro-char #\{ '|{-reader|)
  (:macro-char #\} (get-macro-character #\) (copy-readtable nil))))

(defun read-json (&optional stream errorp return)
  (let ((*readtable* (named-readtables:find-readtable 'rpjson)))
    (read stream errorp return)))

;;;; PRINTER

(defun jprint-null (stream exp) (write-string "\"null\"" stream) exp)

(defun jprint-true (stream exp) (write-string "\"true\"" stream) exp)

(defun jprint-false (stream exp) (write-string "\"false\"" stream) exp)

(defun jprint-keyword (stream exp)
  (write-string (format nil "\"~A\"" (symbol-name exp)) stream)
  exp)

(defparameter *indent* 4)

(defparameter *nest* 0)

(defun jprint-vector (stream exp)
  (write-char #\[ stream)
  (let* ((*nest* (1+ *nest*)))
    (loop :for i :upfrom 0
          :initially (write-char #\Space stream)
                     (pprint-indent :block (* *indent* *nest*) stream)
                     (pprint-newline :linear stream)
          :if (array-in-bounds-p exp i)
            :do (write (aref exp i) :stream stream)
          :else
            :do (loop-finish)
          :if (array-in-bounds-p exp (1+ i))
            :do (write-char #\, stream)
                (write-char #\Space stream)
                (pprint-newline :linear stream)
          :else
            :do (write-char #\Space stream)
                (pprint-indent :block (* *indent* (1- *nest*)) stream)
                (pprint-newline :linear stream)))
  (write-char #\] stream)
  exp)

(defun jprint-object (stream exp)
  (write-char #\{ stream)
  (let* ((*nest* (1+ *nest*)))
    (with-hash-table-iterator (get-it exp)
      (labels ((rec (count)
                 (case count
                   (0)
                   (1
                    (multiple-value-call #'put (get-it))
                    (write-char #\Space stream)
                    (pprint-indent :block (* *indent* (1- *nest*)) stream)
                    (pprint-newline :linear stream))
                   (otherwise
                    (multiple-value-call #'put (get-it))
                    (write-char #\, stream)
                    (write-char #\Space stream)
                    (pprint-newline :mandatory stream)
                    (rec (1- count)))))
               (put (found? key v)
                 (declare (ignore found?))
                 (let ((*nest* 0))
                   (pprint-logical-block (stream nil)
                     (write key :stream stream)
                     (write-char #\: stream)
                     (write-char #\Space stream)
                     (write v :stream stream)))))
        (write-char #\Space stream)
        (pprint-indent :block (* *nest* *indent*) stream)
        (pprint-newline :linear stream)
        (rec (hash-table-count exp)))))
  (write-char #\} stream)
  exp)

(defparameter *print-jprint-dispatch*
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
    (set-pprint-dispatch '(eql null) 'jprint-null)
    (set-pprint-dispatch '(eql t) 'jprint-true)
    (set-pprint-dispatch '(eql nil) 'jprint-false)
    (set-pprint-dispatch 'keyword 'jprint-keyword)
    (set-pprint-dispatch '(and vector (not string)) 'jprint-vector)
    (set-pprint-dispatch 'hash-table 'jprint-object)
    *print-pprint-dispatch*))

(defun print-json (exp &optional stream)
  (let ((*print-pprint-dispatch* *print-jprint-dispatch*) (*nest* *nest*))
    (pprint-logical-block (stream nil) (write exp :stream stream)))
  exp)
