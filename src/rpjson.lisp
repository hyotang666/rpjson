(in-package :cl-user)

(defpackage :rpjson
  (:use :cl)
  (:export ;; Main api.
           #:read-json
           #:print-json
           ;; Readtable name.
           #:rpjson
           ;; Config
           #:*indent*))

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
  (write-char #\" stream)
  (write-string (symbol-name exp) stream)
  (write-char #\" stream)
  exp)

(defparameter *indent* 4)

(defparameter *nest* 0)

(defun jprint-vector (stream exp)
  (write-char #\[ stream)
  (let ((*nest* (1+ *nest*)))
    (loop :for i :upfrom 0
          :initially (funcall (formatter " ~VI~_") stream (* *indent* *nest*))
          :if (array-in-bounds-p exp i)
            :do (write (aref exp i) :stream stream)
          :else
            :do (loop-finish)
          :if (array-in-bounds-p exp (1+ i))
            :do (funcall (formatter ", ~_") stream)
          :else
            :do (funcall (formatter " ~VI~_") stream
                         (* *indent* (1- *nest*)))))
  (write-char #\] stream)
  exp)

(defun jprint-object (stream exp)
  (write-char #\{ stream)
  (let ((*nest* (1+ *nest*)))
    (with-hash-table-iterator (get-it exp)
      (labels ((rec (count)
                 (case count
                   (0)
                   (1
                    (multiple-value-call #'put (get-it))
                    (funcall (formatter " ~VI~_") stream
                             (* *indent* (1- *nest*))))
                   (otherwise
                    (multiple-value-call #'put (get-it))
                    (funcall (formatter ", ~:@_") stream)
                    (rec (1- count)))))
               (put (found? key v)
                 (declare (ignore found?))
                 (pprint-logical-block (stream nil)
                   (let ((*nest* 0))
                     (funcall (formatter "~W: ~W") stream key v)))))
        (funcall (formatter " ~VI~_") stream (* *nest* *indent*))
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
    (write exp :stream stream))
  exp)
