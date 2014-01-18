;;; -*- lexical-binding: t -*-

(cl-defmacro gnu-apl--with-temp-files (files &body body)
  (declare (indent 1))
  (if files
      (let ((f (car files))
            (temp-file-name (gensym)))
        `(let ((,temp-file-name (make-temp-file ,(cadr f))))
           (let ((,(car f) ,temp-file-name))
             (unwind-protect
                 (gnu-apl--with-temp-files ,(cdr files)
                   ,@body)
               (delete-file ,temp-file-name)))))
    `(progn ,@body)))

(cl-defmacro gnu-apl--define-variable-reading-function ((fun-name varname) &body body)
  (declare (indent 1))
  (let ((result-sym (gensym "result-")))
    `(defun gnu-apl-plot-variable (varname)
       (interactive (list (gnu-apl--choose-variable "Variable: " :variable)))
       (gnu-apl--send-network-command (concat "getvar:" name))
       (let ((,result-sym (gnu-apl--read-network-reply-block)))
         (unless (string= (car ,result-sym) "content")
           (error "Unable to read variable. Response: %s" (car result)))
         (let ((,varname (car (read-from-string (apply #'concat (cdr ,result-sym))))))
           ,@body)))))

(defun gnu-apl--single-dimension-p (value)
  "Returns non-nil if VALUE is a single-dimension array returned
from the APL runtime"
  (and (listp value)
       (or (numberp (car value))
           (listp (car value)))))

(gnu-apl--define-variable-reading-function (gnu-apl-plot-line result)
  (unless (gnu-apl--single-dimension-p result)
    (user-error "Line plots are only supported for one-dimensional values"))
  (gnu-apl--with-temp-files ((script-file "script")
                             (data-file "data"))
    (with-temp-buffer
      (dolist (entry result)
        (etypecase entry
          (integer (insert (format "%d" entry)))
          (number (insert (format "%f" entry)))
          (string (insert (format "\"%s\"" entry))))
        (insert "\n"))
      (write-file data-file))
    (with-temp-buffer
      (insert "plot \"" data-file "\" title \"Data from variable\" with lines\n")
      (write-file script-file))))
