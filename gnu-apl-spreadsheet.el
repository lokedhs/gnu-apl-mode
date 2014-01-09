;;; -*- lexical-binding: t -*-

(defun gnu-apl-edit-variable (name)
  (interactive (list (gnu-apl--choose-variable "Variable: " :variable)))
  (gnu-apl--send-network-command (concat "getvar:" name))
  (let ((result (gnu-apl--read-network-reply-block)))
    (unless (string= (car result) "content")
      (error "Unable to read variable. Response: %s" (car result)))
    (let ((value (car (read-from-string (apply #'concat (cdr result))))))
      (cond ((and (eq (car value) :vector)
                  (= (length (cadr value)) 2))
             (gnu-apl--edit-value-in-spreadsheet name value))
            (t
             (error "Unable to edit values of this type"))))))

(defun gnu-apl-spreadsheet-send-this-document ()
  (interactive)
  (error "Not implemented"))

(define-minor-mode gnu-apl-spreadsheet-mode
  "A variation of `ses-mode' to be used for editing APL matrices."
  nil
  " ≡"
  (list (cons (kbd "C-c C-c") 'gnu-apl-spreadsheet-send-this-document)
        (cons [menu-bar gnu-apl] (cons "APL" (make-sparse-keymap "APL")))
        (cons [menu-bar gnu-apl send-this-document] '("Send document" . gnu-apl-spreadsheet-send-this-document))
        (cons [menu-bar gnu-apl copy-spreadsheet-as-apl-function] '("Copy document as function". gnu-apl-copy-spreadsheet-to-kill-ring)))
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
                                   (list (etypecase (car (col-content))
                                           (symbol (format "!%s" (car col-content)))
                                           (list "!list")))
                                   (t (error "Illegal cell content: %S" col-content)))))
                          (ses-edit-cell row-index col-index v)))))))

(defun gnu-apl-copy-spreadsheet-to-kill-ring (function-name)
  "Copy a function definition representing the data in the active
SES spreadhsheet into the kill ring. When executed, this function
returns the content of the spreadsheet."
  (interactive "sFunction name: ")
  (kill-new (concat "∇" (gnu-apl-make-function-from-spreadsheet-data function-name) "\n∇")))

(defun gnu-apl-make-function-from-spreadsheet-data (function-name)
  "Return an APL function with name NAME that that returns an APL
array with the same values as the spreadsheet in the current
buffer."
  (concat "Z←" function-name "\n" (gnu-apl-make-array-loading-instructions "Z")))

(defun gnu-apl-make-array-loading-instructions (var-name)
  (with-output-to-string
    (let ((rows ses--numrows)
          (cols ses--numcols))
      (princ (format "%s←%d⍴0\n" var-name (* rows cols)))
      (loop for row from 0 below rows
            do (progn
                 (princ (format "%s[%d+⍳%d]←" var-name (* row cols) cols))
                 (loop for col from 0 below cols
                       do (let ((item (ses-cell-value row col)))
                            (typecase item
                              (number (princ item))
                              (string (princ (format "'%s'" (replace-regexp-in-string "'" "''" item))))
                              (t (ses-goto-print row col) (error "Invalid content in cell %d,%d" row col)))
                            (if (< col (1- cols))
                                (princ " ")
                              (princ "\n"))))))
      (princ (format "%s←(%d %d)⍴%s" var-name rows cols var-name)))))
