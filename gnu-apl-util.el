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
