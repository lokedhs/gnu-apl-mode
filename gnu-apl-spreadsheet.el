;;; -*- lexical-binding: t -*-

(defun ses-test ()
  (let ((v '(:vector (4 5)
                     ((38 9 33 82 12)
                      (40 68 54 7 84)
                      (16 98 65 86 87)
                      (60 43 74 47 18)))))
    (gnu-apl--edit-value-in-spreadsheet "foo" v)))

(defun gnu-apl-spreadsheet-send-this-document ()
  (interactive)
  (error "Not implemented"))

(define-minor-mode gnu-apl-spreadsheet-mode
  "A variation of `ses-mode' to be used for editing APL matrices."
  nil
  " ≡"
  (list (cons (kbd "C-c C-c") 'gnu-apl-spreadsheet-send-this-document))
  :group 'gnu-apl)

(defun gnu-apl--edit-value-in-spreadsheet (backend-variable-name value)
  ;; First, ensure that the array is editable
  (unless (and (eq (car value) :vector)
               (= (length (cadr value)) 2))
    (error "Only two-dimensional arrays can be edited"))
  (let* ((buffer-name (format "*gnu-apl array %s*" backend-variable-name))
         (buffer (get-buffer buffer-name)))
    (when buffer
      (kill-buffer buffer))
    (setq buffer (get-buffer-create buffer-name))
    (switch-to-buffer buffer)
    (ses-mode)
    (gnu-apl-spreadsheet-mode)
    (let* ((dimensions (cadr value))
           (rows (car dimensions))
           (cols (cadr dimensions)))
      (ses-insert-row (1- rows))
      (ses-insert-column (1- cols))
      (loop for row-index from 0 below rows
            for row-values in (caddr value)
            do (loop for col-index from 0 below cols
                     for col-content in row-values
                     do (let ((v (etypecase col-content
                                   (integer col-content)
                                   (float col-content)
                                   (string col-content)
                                   (t (error "Illegal cell content: %S" col-content)))))
                          (ses-edit-cell row-index col-index v)))))))
