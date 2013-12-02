;;; -*- lexical-binding: t -*-

(defvar gnu-apl-current-session nil
  "The buffer that holds the currently active GNU APL session,
or NIL if there is no active session.")

(defvar gnu-apl-interactive-mode-map
  (gnu-apl--make-mode-map "s-"))

(defun gnu-apl-interactive-send-string (string)
  (let ((proc-status (comint-check-proc gnu-apl-current-session)))
    (unless (eq (car proc-status) 'run)
      (error "No active GNU APL session")))
  (let ((p (get-buffer-process gnu-apl-current-session))
        (string-with-ret (if (= (aref string (1- (length string))) ?\n)
                             string
                           (concat string "\n"))))
    ;(comint-skip-input)
    (comint-send-string p string-with-ret)))

(defun gnu-apl-interactive-send-region (start end)
  (interactive "r")
  (gnu-apl-interactive-send-string (buffer-substring start end))
  (message "Region sent to APL"))

(define-derived-mode gnu-apl-interactive-mode comint-mode "GNU APL/Comint"
  "Major mode for interacting with GNU APL."
  :syntax-table gnu-apl-mode-syntax-table
  :group 'gnu-apl
  (use-local-map gnu-apl-interactive-mode-map)
  (setq comint-prompt-regexp "^\\(      \\)\\|\\(\\[[0-9]+\\] \\)")
  (setq comint-process-echoes nil))

(defun gnu-apl ()
  (interactive)
  (let ((buffer (get-buffer-create "*gnu-apl*")))
    (pop-to-buffer-same-window buffer)
    (unless (comint-check-proc buffer)
      (make-comint-in-buffer "apl" buffer gnu-apl-executable nil
                             "--noCIN" "--noColor")
      (gnu-apl-interactive-mode)
      (setq gnu-apl-current-session buffer))))
