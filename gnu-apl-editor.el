;;; -*- lexical-binding: t -*-

(defun gnu-apl-interactive-send-region (start end)
  (interactive "r")
  (gnu-apl-interactive-send-string (buffer-substring start end))
  (message "Region sent to APL"))

(defun gnu-apl-interactive-send-current-function ()
  (interactive)

  (labels ((full-function-definition-p (line)
                                       (when (and (plusp (length line))
                                                  (string= (subseq line 0 1) "∇"))
                                         (let ((parsed (gnu-apl--parse-function-header (subseq line 1))))
                                           (unless parsed
                                             (user-error "Function end marker above cursor"))
                                           parsed))))

    (save-excursion
      (beginning-of-line)
      (let ((start (loop for line = (gnu-apl--trim-spaces (thing-at-point 'line))
                         when (full-function-definition-p line)
                         return (point)
                         when (plusp (forward-line -1))
                         return nil)))
        (unless start
          (user-error "Can't find function definition above cursor"))

        (unless (zerop (forward-line 1))
          (user-error "No end marker found"))
        (let ((end (loop for line = (gnu-apl--trim-trailing-newline
                                     (gnu-apl--trim-spaces (thing-at-point 'line)))
                         when (string= line "∇")
                         return (progn (forward-line -1) (end-of-line) (point))
                         when (plusp (forward-line 1))
                         return nil)))
          (unless end
            (user-error "No end marker found"))
          (let ((overlay (make-overlay start end)))
            (overlay-put overlay 'face '(background-color . "green"))
            (run-at-time "0.5 sec" nil #'(lambda () (delete-overlay overlay))))
          (gnu-apl--send-si-and-send-new-function (buffer-substring start end) nil))))))

(defun gnu-apl--send-si-and-send-new-function (content edit-when-fail)
  "Send an )SI request that should be checked against the current
function being sent."
  (with-current-buffer (gnu-apl--get-interactive-session)
    (let ((parts (split-string content "\n")))
      (unless parts
        (error "Missing content"))
      (let ((trimmed (gnu-apl--trim-spaces (car parts))))
        (unless (string= (subseq trimmed 0 1) "∇")
          (error "Illegal function header format"))
        (let ((function-header (subseq trimmed 1)))
          (unless (gnu-apl--parse-function-header function-header)
            (error "Unable to parse function header"))
          (setq gnu-apl-current-function-title function-header)
          (setq gnu-apl-content content)
          (setq gnu-apl-edit-when-si-fail edit-when-fail)
          (gnu-apl-interactive-send-string (concat "'" *gnu-apl-read-si-start* "'"))
          (gnu-apl-interactive-send-string (concat ")SI"))
          (gnu-apl-interactive-send-string (concat "'" *gnu-apl-read-si-end* "'")))))))

(defun gnu-apl-save-function ()
  "Save the currently edited function."
  (interactive)
  (goto-char (point-min))
  (let ((definition (gnu-apl--trim-spaces (thing-at-point 'line))))
    (unless (string= (subseq definition 0 1) "∇")
      (user-error "Function header does not start with function definition symbol"))
    (unless (zerop (forward-line))
      (user-error "Empty function definition"))
    (let* ((function-header (subseq definition 1))
           (function-name (gnu-apl--parse-function-header function-header)))
      (unless function-name
        (user-error "Illegal function header"))

      ;; Ensure that there are no function-end markers in the buffer
      ;; (unless it's the last character in the buffer)
      (let* ((end-of-function (if (search-forward "∇" nil t)
                                  (1- (point))
                                (point-max)))
             (buffer-content (buffer-substring (point-min) end-of-function))
             (content (if (eql (aref buffer-content (1- (length buffer-content))) ?\n)
                          buffer-content
                        (concat buffer-content "\n"))))

        ;; At this point, we have the following needed information:
        ;;   function-header: the first line of the definition (minus the function definition symbol)
        ;;   content: the function definition itself as a single string
        ;;
        ;; Now, we need to first check the )SI stack to make sure there is no
        ;; active definition already (and take appropriate action), and then send
        ;; the function to the APL interpreter.
        (gnu-apl--send-si-and-send-new-function content t))

      (let ((window-configuration (if (boundp 'gnu-apl-window-configuration)
                                      gnu-apl-window-configuration
                                    nil)))
        (kill-buffer (current-buffer))
        (when window-configuration
          (set-window-configuration window-configuration))))))

(define-minor-mode gnu-apl-interactive-edit-mode
  "Minor mode for editing functions in the GNU APL function editor"
  nil
  " APLFunction"
  (list (cons (kbd "C-c C-c") 'gnu-apl-save-function))
  :group 'gnu-apl)

(defun gnu-apl--open-function-editor-with-timer (lines)
  (run-at-time "0 sec" nil #'(lambda () (gnu-apl-open-external-function-buffer lines))))

(defun gnu-apl-open-external-function-buffer (lines)
  (let ((window-configuration (current-window-configuration))
        (buffer (get-buffer-create "*gnu-apl edit function*")))
    (pop-to-buffer buffer)
    (delete-region (point-min) (point-max))
    (insert "∇")
    (dolist (line lines)
      (insert (gnu-apl--trim-spaces line nil t))
      (insert "\n"))
    (goto-char (point-min))
    (forward-line 1)
    (gnu-apl-mode)
    (gnu-apl-interactive-edit-mode 1)
    (set (make-local-variable 'gnu-apl-window-configuration) window-configuration)
    (message "To save the buffer, use M-x gnu-apl-save-function (C-c C-c)")))
