;;; -*- lexical-binding: t -*-

(defun gnu-apl--make-trace-buffer-name (varname)
  (format "*gnu-apl trace %s*" varname))

;;;
;;;  gnu-apl-trace-symbols is a list of traced symbols, each element
;;;  has the following structure:
;;;
;;;    ("symbol_name" <buffer>)
;;;

(defun gnu-apl--find-traced-symbol (varname)
  (cl-find varname gnu-apl-trace-symbols :key #'car :test #'string=))

(defun gnu-apl--cleanup-trace-symbol (buffer)
  (with-current-buffer buffer
    (when (boundp 'gnu-apl-trace-symbols)
      (dolist (sym gnu-apl-trace-symbols)
        (when (buffer-live-p (cadr sym))
          (with-current-buffer (cadr sym)
            (when (boundp 'gnu-apl-trace-variable)
              (setq gnu-apl-trace-variable nil))))))))

(defun gnu-apl--trace-buffer-closed ()
  (let ((varname gnu-apl-trace-variable))
    (when varname
      (with-current-buffer (gnu-apl--get-interactive-session)
        (let ((traced (gnu-apl--find-traced-symbol varname)))
          (when traced
            (setq gnu-apl-trace-symbols (cl-remove (car traced) gnu-apl-trace-symbols :key #'car :test #'string=))
            (let ((result (gnu-apl--send-network-command-and-read (format "trace:%s:off" (car traced)))))
              (unless (and result (string= (car result) "disabled"))
                (error "Symbol was not traced")))))))))

(defun gnu-apl--trace-symbol-updated (content)
  (let ((varname (car content)))
    (let ((traced (gnu-apl--find-traced-symbol varname)))
      (when traced
        (with-current-buffer (cadr traced)
          (widen)
          (let ((pos (line-number-at-pos (point))))
            (delete-region (point-min) (point-max))
            (dolist (row (cdr content))
              (insert row "\n"))
            (goto-char (point-min))
            (forward-line (1- pos))))))))

(defun gnu-apl-trace (varname)
  (interactive (list (gnu-apl--choose-variable "Variable: " :variable)))
  (with-current-buffer (gnu-apl--get-interactive-session)
    (let ((traced (gnu-apl--find-traced-symbol varname)))
      (let ((b (if traced
                   (cadr traced)
                 (let ((result (gnu-apl--send-network-command-and-read (format "trace:%s:on" varname))))
                   (unless (and result (string= (car result) "enabled"))
                     (error "Unexpected response from trace command"))
                   (let ((buffer (generate-new-buffer (gnu-apl--make-trace-buffer-name varname))))
                     (with-current-buffer buffer
                       (set (make-local-variable 'gnu-apl-trace-variable) varname)
                       (add-hook 'kill-buffer-hook 'gnu-apl--trace-buffer-closed nil t))
                     (push (list varname buffer) gnu-apl-trace-symbols)
                     buffer)))))
        (switch-to-buffer-other-window b)))))
