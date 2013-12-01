;;; -*- lexical-binding: t -*-

(defvar gnu-apl-interactive-mode-map
  (gnu-apl--make-mode-map "s-"))

(define-derived-mode gnu-apl-interactive-mode comint-mode "GNU APL/Comint"
  "Major mode for interacting with GNU APL."
  :syntax-table gnu-apl-mode-syntax-table
  :group 'gnu-apl
  (use-local-map gnu-apl-interactive-mode-map)
  (setq comint-prompt-regexp "^      ")
  (setq comint-process-echoes t))

(defun gnu-apl ()
  (interactive)
  (let ((buffer (get-buffer-create "*gnu-apl*")))
    (pop-to-buffer-same-window buffer)
    (unless (comint-check-proc buffer)
      (make-comint-in-buffer "apl" buffer gnu-apl-executable
                             nil
                             "--noCIN")
      (gnu-apl-interactive-mode))))
