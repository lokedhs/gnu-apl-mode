;;; -*- lexical-binding: t -*-

(defvar gnu-apl-current-session nil
  "The buffer that holds the currently active GNU APL session,
or NIL if there is no active session.")

(defun gnu-apl-interactive-send-string (string)
  (let ((p (get-buffer-process (gnu-apl--get-interactive-session)))
        (string-with-ret (if (= (aref string (1- (length string))) ?\n)
                             string
                           (concat string "\n"))))
    ;(comint-skip-input)
    (comint-send-string p string-with-ret)))

(defun gnu-apl-interactive-send-region (start end)
  (interactive "r")
  (gnu-apl-interactive-send-string (buffer-substring start end))
  (message "Region sent to APL"))

(defun gnu-apl--get-interactive-session ()
  (unless gnu-apl-current-session
    (user-error "No active GNU APL session"))
  (let ((proc-status (comint-check-proc gnu-apl-current-session)))
    (unless (eq (car proc-status) 'run)
      (user-error "GNU APL session has exited"))
    gnu-apl-current-session))

(defvar *gnu-apl-function-text-start* "START")
(defvar *gnu-apl-function-text-end* "END")

(defun gnu-apl-edit-function (name)
  (interactive "MFunction name: ")
  (gnu-apl--get-function name))

(defun gnu-apl--get-function (function)
  (with-current-buffer (gnu-apl--get-interactive-session)
    (let ((max (point-max)))
      (setq gnu-apl-current-function-text nil)
      (gnu-apl-interactive-send-string (concat "'" *gnu-apl-function-text-start*
                                               "' ⋄ ⎕CR '" function
                                               "' ⋄ '" *gnu-apl-function-text-end* "'"))
      )))

(defun gnu-apl--preoutput-filter (line)
  (with-output-to-string
    (loop with first = t
          for plain in (split-string line "\n")
          collect (cond (gnu-apl-reading-function
                         (if (string= plain *gnu-apl-function-text-end*)
                             (progn
                               (setq gnu-apl-reading-function nil)
                               (let ((s gnu-apl-current-function-text))
                                 (setq gnu-apl-current-function-text nil)
                                 (setq gnu-apl-reading-function nil)
                                 (gnu-apl--open-function-editor-with-timer s)))
                           (setq gnu-apl-current-function-text
                                 (append gnu-apl-current-function-text (list plain)))))
                        ((and (not gnu-apl-reading-function)
                              (>= (length plain) (length *gnu-apl-function-text-start*))
                              (string= (subseq plain (- (length plain) (length *gnu-apl-function-text-start*)))
                                       *gnu-apl-function-text-start*))
                         (setq gnu-apl-reading-function t)
                         (setq gnu-apl-current-function-text nil))
                        (t
                         (if first
                             (setq first nil)
                           (princ "\n"))
                         (princ plain))))))

(defvar gnu-apl-interactive-mode-map
  (let ((map (gnu-apl--make-mode-map "s-")))
    (define-key map (kbd "C-c f") 'gnu-apl-edit-function)
    map))

(define-derived-mode gnu-apl-interactive-mode comint-mode "GNU APL/Comint"
  "Major mode for interacting with GNU APL."
  :syntax-table gnu-apl-mode-syntax-table
  :group 'gnu-apl
  (use-local-map gnu-apl-interactive-mode-map)
  (setq comint-prompt-regexp "^\\(      \\)\\|\\(\\[[0-9]+\\] \\)")
  ;;(setq comint-prompt-regexp "^\\(      \\)?")
  ;(setq comint-process-echoes t)
  (set (make-local-variable 'gnu-apl-current-function-text) nil)
  (set (make-local-variable 'gnu-apl-reading-function) nil)
  (add-hook 'comint-preoutput-filter-functions 'gnu-apl--preoutput-filter nil t)
  (set (make-local-variable 'comint-input-sender) 'gnu-apl--send))

(defun gnu-apl ()
  (interactive)
  (let ((buffer (get-buffer-create "*gnu-apl*")))
    (pop-to-buffer-same-window buffer)
    (unless (comint-check-proc buffer)
      (make-comint-in-buffer "apl" buffer gnu-apl-executable nil
                             "--noCIN" "--noColor")
      (gnu-apl-interactive-mode)
      (setq gnu-apl-current-session buffer))))

(defun gnu-apl--open-function-editor-with-timer (lines)
  (run-at-time "0 sec" nil #'(lambda () (gnu-apl-open-external-function-buffer lines))))

(defun gnu-apl-open-external-function-buffer (lines)
  (let ((buffer (get-buffer-create "*gnu-apl edit function*")))
    (pop-to-buffer buffer)
    (delete-region (point-min) (point-max))
    (insert "∇")
    (dolist (line lines)
      (insert line)
      (insert "\n"))
    (goto-char (point-min))
    (forward-line 1)
    (gnu-apl-mode)
    (local-set-key (kbd "C-c C-c") 'gnu-apl-save-function)))

(defun gnu-apl--parse-function-header (string)
  (let ((line-fix (gnu-apl--trim "[ \t]" string)))
    (when (and (> (length line-fix) 0)
               (string= (char-to-string (aref line-fix 0)) "∇"))
      (let ((line (subseq line-fix 1)))
        (when (string-match (concat "^ *\\(?:\\([a-z0-9∆]+\\) *← *\\)?" ; result variable
                                    "\\([a-za-z0-9∆ ]+\\)" ; function and arguments
                                    "\\(;.*\\)?$" ; local variables
                                    )
                            line)
          (let ((result-variable (match-string 1 line))
                (function-and-arguments (match-string 2 line))
                (local-variables (match-string 3 line)))
            (let ((parts (split-string function-and-arguments)))
              (when (< 0 (length parts) 4)
                (append (list result-variable)
                        (ecase (length parts)
                          (1 (list nil (car parts) nil))
                          (2 (list nil (car parts) (cadr parts)))
                          (3 (list (car parts) (cadr parts) (caddr parts)))))))))))))

(defun gnu-apl-save-function ()
  (interactive)
  (goto-char (point-min))
  (let ((definition (thing-at-point 'line)))
    (unless (zerop (forward-line))
      (user-error "Empty function definition"))
    (let ((function-arguments (gnu-apl--parse-function-header definition)))
      (unless function-arguments
        (user-error "Illegal function header"))

      ;; Ensure that there are no function-end markers in the buffer
      ;; (unless it's the last character in the buffer)
      (let* ((end-of-function (if (search-forward "∇" nil t)
                                  (1- (point))
                                (point-max)))
             (buffer-content (buffer-substring (point-min) end-of-function))
             (content (if (eql (aref buffer-content (1- (length buffer-content))) ?\n)
                          (concat buffer-content "\n")
                        buffer-content)))

        (gnu-apl-interactive-send-string (concat ")ERASE " (caddr function-arguments)))
        (gnu-apl-interactive-send-string (concat content "\n∇\n"))
        (kill-buffer (current-buffer))))))

(defun gnu-apl--send (proc string)
  (let ((parsed (gnu-apl--parse-function-header string)))
    (if parsed
        (gnu-apl--get-function (caddr parsed))
      (comint-simple-send proc string))))
