":" ; exec cl-launch -Q -s fortunes -p fortunes/manage -E main -- "$@"

(defpackage :fortunes/manage
  (:use :cl))

(in-package :fortunes/manage)

(declaim (optimize (debug 3)))

(opts:define-opts
  (:name :help
         :description "print this help text"
         :short #\h
         :long "help")
  (:name :parse
         :description "parse to sexp"
         :short #\p
         :long "parse")
  (:name :compile
         :description "compile to fortune file"
         :short #\c
         :long "compile"))

(defun read-lines-by-block (in-stream &optional (block-size 4096))
  (let ((scratch (make-string block-size))
        (list '()))
    (loop
       for read = (read-sequence scratch in-stream)
       while (plusp read) 
       for part = (loop for start = 0 then (1+ pos)
                     while (< start read)
                     for pos = (position #\Newline scratch :start start)
                     for next = (subseq scratch start pos)
                     while pos
                     do (push (if (zerop start)
                                  (concatenate 'string part next)
                                  next)
                              list)
                     finally (return (if (zerop start)
                                         (concatenate 'string part next)
                                         (and (< start read) next))))
       finally (return
                 (if (plusp (length part))
                     (push part list)
                     list)))))

(defun read-lines (in-stream)
  (loop for line = (read-line in-stream nil)
     while line collect line))

(defun trim (string)
  (string-trim #(#\Newline #\Return #\Space) string))

(defun slurp-fortunes (pathname)
  (let* ((lines
          (with-open-file (in pathname)
            (read-lines in)))
         (clean-lines
          (mapcar #'trim lines))
         (fortunes
          (reduce (lambda (list string)
                    (destructuring-bind (&optional (head "") &rest tail) list
                      (if (string= string "%")
                          (cons "" list)
                          (cons (concatenate 'string string #(#\Newline) head) tail))))
                  (nreverse clean-lines) :initial-value nil))
         (clean-fortunes
          (remove-duplicates (mapcar #'trim fortunes) :test #'string=)))
    clean-fortunes))


(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defun main (argv)
  (declare (ignore argv))
  (multiple-value-bind (options free-args)
      (handler-case
          (handler-bind ((opts:unknown-option #'unknown-option))
            (opts:get-opts))
        (opts:missing-arg (condition)
          (format t "fatal: option ~s needs an argument!~%"
                  (opts:option condition)))
        (opts:arg-parser-failed (condition)
          (format t "fatal: cannot parse ~s as argument of ~s~%"
                  (opts:raw-arg condition)
                  (opts:option condition))))

    (cond
      ((and (getf options :parse)
            (getf options :compile))
       (format *error-output* "Parse or compile, not both~%"))
      
      ((getf options :parse)
       (loop for fortune in (reduce (lambda (acc pathname)
                                      (union acc (slurp-fortunes pathname)))
                                    free-args :initial-value nil)
          do (print fortune)))
      
      ((getf options :compile)
       (format t "~{~{~a~%~^%~%~}~^%~%~}"
               (loop for filename in free-args
                  collect (with-open-file (in (parse-namestring filename))
                            (loop for fortune = (read in nil)
                               while fortune collect fortune)))))
      
      (t (opts:describe
          :prefix "A tool for managing collections of fortune cookies with the power of Lisp"
          :suffix "Thank you for using a CoselTech product"
          :usage-of "manage.lisp"
          :args     "[FREE-ARGS]")))))
