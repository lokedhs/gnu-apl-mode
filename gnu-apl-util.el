;;; -*- lexical-binding: t -*-

(require 'cl)

(cl-defun gnu-apl--trim (regexp string &optional (start t) (end t))
  (if (or start end)
    (let ((res string)
          (reg (cond ((and start end)
                      (concat "\\(^" regexp "\\)\\|\\(" regexp "$\\)"))
                     ((not end)
                      (concat "^" regexp))
                     (t
                      (concat regexp "$")))))
      (while (string-match reg res)
        (setq res (replace-match "" t t res)))
      res)
    string))

(cl-defun gnu-apl--trim-spaces (string &optional (start t) (end t))
  (gnu-apl--trim "[ \t]" string start end))

(cl-defun gnu-apl--trim-trailing-newline (string)
  (gnu-apl--trim "[\n\r]" string nil t))

(defun gnu-apl--open-new-buffer (name)
  (let ((buffer (get-buffer name)))
    (when buffer
      (delete-windows-on buffer)
      (kill-buffer buffer))
    (get-buffer-create name)))
