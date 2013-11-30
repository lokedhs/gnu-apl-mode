;;; -*- lexical-binding: t -*-

(define-derived-mode interactive-gnu-apl-mode comint-mode "Interactive GNU APL"
  "Major mode for interacting with GNU APL."
  :syntax-table gnu-apl-mode-syntax-table
  :group 'gnu-apl-mode
  (use-local-map gnu-apl-mode-map)
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
      (interactive-gnu-apl-mode))))
