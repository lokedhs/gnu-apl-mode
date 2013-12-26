;;; -*- lexical-binding: t -*-

(defvar gnu-apl-current-session nil
  "The buffer that holds the currently active GNU APL session,
or NIL if there is no active session.")

(defun gnu-apl-interactive-send-string (string)
  (let ((p (get-buffer-process (gnu-apl--get-interactive-session)))
        (string-with-ret (if (= (aref string (1- (length string))) ?\n)
                             string
                           (concat string "\n"))))
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

(defvar *gnu-apl-function-text-start* "FUNCTION-CONTENT-START")
(defvar *gnu-apl-function-text-end* "FUNCTION-CONTENT-END")
(defvar *gnu-apl-ignore-start* "IGNORE-START")
(defvar *gnu-apl-ignore-end* "IGNORE-END")
(defvar *gnu-apl-read-si-start* "READ-SI-STATUS-START")
(defvar *gnu-apl-read-si-end* "READ-SI-STATUS-END")

(defun gnu-apl-edit-function (name)
  "Open the function with the given name in a separate buffer.
After editing the function, use `gnu-apl-save-function' to save
the function and set it in the running APL interpreter."
  (interactive "MFunction name: ")
  (gnu-apl--get-function name))

(defun gnu-apl--get-function (function-definition)
  (setq gnu-apl-current-function-title function-definition)
  (gnu-apl-interactive-send-string (concat "'" *gnu-apl-read-si-start* "'"))
  (gnu-apl-interactive-send-string (concat ")SI"))
  (gnu-apl-interactive-send-string (concat "'" *gnu-apl-read-si-end* "'")))

(defun gnu-apl--parse-text (string)
  (if (zerop (length string))
      (list 'normal string)
    (let ((char (aref string 0))
          (command (subseq string 1)))
      (case char
        (#xf00c0 (list 'cin command))
        (#xf00c1 (list 'cout command))
        (#xf00c2 (list 'cerr command))
        (#xf00c3 (list 'app-error command))
        (t (list 'normal string))))))

(defun gnu-apl--set-face-for-text (type text)
  (let ((s (copy-seq text)))
    (case type
      (cerr (add-text-properties 0 (length s) '(font-lock-face gnu-apl-error-face) s))
      (app-error (add-text-properties 0 (length s) '(font-lock-face gnu-apl-user-status-text-face) s)))
    s))

(defun gnu-apl--process-si-line (line)
  (when (string-match (concat "^\\(?:\r      .\\)?\\([a-zA-Z0-9_∆]+\\)\\[[0-9]+\\]") line)
    (match-string 1 line)))

(defun gnu-apl--process-si-lines (lines)
  (loop for line in lines
        for processed = (gnu-apl--process-si-line line)
        when processed
        collect processed))

(defun gnu-apl--preoutput-filter (line)
  (let ((result ""))
    (labels ((add-to-result (s)
               (setq result (concat result s)))

             (send-edit-function (title)
               (gnu-apl-interactive-send-string (concat "'" *gnu-apl-function-text-start*
                                                        "' ⋄ ⎕CR '" title
                                                        "' ⋄ '" *gnu-apl-function-text-end* "'"))))

      (loop with first = t
            for plain in (split-string line "\n")
            do (destructuring-bind (type command) (gnu-apl--parse-text plain)
                 (ecase gnu-apl-preoutput-filter-state
                   ;; Default parse state
                   (normal (cond ((string-match *gnu-apl-function-text-start* command)
                                  (setq gnu-apl-current-function-text nil)
                                  (setq gnu-apl-preoutput-filter-state 'reading-function))
                                 ((string-match *gnu-apl-ignore-start* command)
                                  (setq gnu-apl-preoutput-filter-state 'ignore))
                                 ((string-match *gnu-apl-read-si-start* command)
                                  (setq gnu-apl-preoutput-filter-state 'read-si))
                                 (t
                                  (if first
                                      (setq first nil)
                                    (add-to-result "\n"))
                                  (add-to-result (gnu-apl--set-face-for-text type command)))))
                   ;; Reading the content of a function
                   (reading-function (cond ((string-match *gnu-apl-function-text-end* command)
                                            (let ((s (cond (gnu-apl-current-function-text
                                                            (reverse gnu-apl-current-function-text))
                                                           (gnu-apl-current-function-title
                                                            (list gnu-apl-current-function-title))
                                                           (t
                                                            (error "No function content found and title was not set")))))
                                              (setq gnu-apl-current-function-text nil)
                                              (setq gnu-apl-current-function-title nil)
                                              (gnu-apl--open-function-editor-with-timer s))
                                            (setq gnu-apl-preoutput-filter-state 'normal))
                                           (t
                                            (push command gnu-apl-current-function-text))))
                   ;; Read the output of )SI
                   (read-si (cond ((string-match *gnu-apl-read-si-end* command)
                                   (unless gnu-apl-current-function-title
                                     (error "End of )SI output but no active function"))
                                   (let ((si (gnu-apl--process-si-lines gnu-apl-current-si)))
                                     (setq gnu-apl-current-si nil)
                                     (labels ((send-edit ()
                                                (send-edit-function gnu-apl-current-function-title))
                                              (send-clear-and-edit ()
                                                (gnu-apl-interactive-send-string ")SIC")
                                                (send-edit)))
                                       (if (cl-find gnu-apl-current-function-title si :test #'equal)
                                           (ecase gnu-apl-redefine-function-when-is-use-action
                                             (error (message "Function already on the )SI stack"))
                                             (clear (send-clear-and-edit))
                                             (allow (send-edit))
                                             (ask (if (y-or-n-p "Function already on )SI stack. Clear )SI stack? ")
                                                      (send-clear-and-edit)
                                                    (send-edit))))
                                         (send-edit)))
                                     (setq gnu-apl-preoutput-filter-state 'normal)))
                                  (t
                                   (push command gnu-apl-current-si))))
                   ;; Ignoring output
                   (ignore (cond ((string-match *gnu-apl-ignore-end* command)
                                  (setq gnu-apl-preoutput-filter-state 'normal))
                                 (t
                                  nil)))))))
    result))

(defvar gnu-apl-interactive-mode-map
  (let ((map (gnu-apl--make-mode-map "s-")))
    (define-key map (kbd "C-c f") 'gnu-apl-edit-function)
    (define-key map [menu-bar gnu-apl edit-function] '("Edit function" . gnu-apl-edit-function))
    map))

(define-derived-mode gnu-apl-interactive-mode comint-mode "GNU APL/Comint"
  "Major mode for interacting with GNU APL."
  :syntax-table gnu-apl-mode-syntax-table
  :group 'gnu-apl
  (use-local-map gnu-apl-interactive-mode-map)
  (gnu-apl--init-mode-common)
  (setq comint-prompt-regexp "^\\(      \\)\\|\\(\\[[0-9]+\\] \\)")

  ;; Holds the current state
  (set (make-local-variable 'gnu-apl-preoutput-filter-state) 'normal)

  ;;
  ;; === Function editor variables
  ;;
  ;; Holds the function name while getting all the SI and function information
  (set (make-local-variable 'gnu-apl-current-function-title) nil)
  ;; List of lines in the function being read (in reverse)
  (set (make-local-variable 'gnu-apl-current-function-text) nil)
  ;; List of the output lines in )SI (reverse)
  (set (make-local-variable 'gnu-apl-current-si) nil)

  (set (make-local-variable 'comint-input-sender) 'gnu-apl--send)
  (add-hook 'comint-preoutput-filter-functions 'gnu-apl--preoutput-filter nil t)
  (setq font-lock-defaults '(nil t)))

(defun gnu-apl-open-customise ()
  (interactive)
  (customize-group 'gnu-apl t))

(defun gnu-apl--insert-tips ()
  (insert "This is the gnu-apl-mode interactive buffer.\n\n"
          "To toggle keyboard help, call M-x gnu-apl-show-keyboard (C-c k by default).\n"
          "APL symbols are bound to the standard keys with the Super key. You can also\n"
          "activate the APL-Z input method (M-x toggle-input-method or C-\\) which\n"
          "allows you to input APL symbols by prefixing the key with a \".\" (period).\n\n"
          "There are several "
          (propertize "customisation"
                      'face 'link
                      'mouse-face 'highlight
                      'help-echo "mouse-2: Customise group gnu-apl"
                      'keymap (let ((keymap (make-sparse-keymap)))
                                (define-key keymap [mouse-2] 'gnu-apl-open-customise)
                                (define-key keymap (kbd "RET") 'gnu-apl-open-customise)
                                keymap))
          " options that can be set.\n"
          "click the link or run M-x customize-group RET gnu-apl to set up.\n\n"
          "To disable this message, set gnu-apl-show-tips-on-start to nil.\n\n"))

(defun gnu-apl (apl-executable)
  (interactive (list (when current-prefix-arg (read-file-name "Location of GNU APL Executable: " nil nil t))))
  (let ((buffer (get-buffer-create "*gnu-apl*"))
        (resolved-binary (or apl-executable gnu-apl-executable)))
    (unless resolved-binary
      (user-error "GNU APL Executable was not set"))
    (pop-to-buffer-same-window buffer)    
    (unless (comint-check-proc buffer)
      (when gnu-apl-show-tips-on-start
        (gnu-apl--insert-tips))
      (apply #'make-comint-in-buffer
             "apl" buffer resolved-binary nil
             "--rawCIN" "--emacs" (append (if (not gnu-apl-show-apl-welcome) (list "--silent"))))
      (gnu-apl-interactive-mode)
      (setq gnu-apl-current-session buffer))
    (when gnu-apl-show-keymap-on-startup
      (run-at-time "0 sec" nil #'(lambda () (gnu-apl-show-keyboard 1))))))

(defun gnu-apl--open-function-editor-with-timer (lines)
  (run-at-time "0 sec" nil #'(lambda () (gnu-apl-open-external-function-buffer lines))))

(defun gnu-apl-open-external-function-buffer (lines)
  (let ((window-configuration (current-window-configuration))
        (buffer (get-buffer-create "*gnu-apl edit function*")))
    (pop-to-buffer buffer)
    (delete-region (point-min) (point-max))
    (insert "∇")
    (dolist (line lines)
      (insert (gnu-apl--trim " " line nil t))
      (insert "\n"))
    (goto-char (point-min))
    (forward-line 1)
    (gnu-apl-mode)
    (local-set-key (kbd "C-c C-c") 'gnu-apl-save-function)
    (set (make-local-variable 'gnu-apl-window-configuration) window-configuration)
    (message "To save the buffer, use M-x gnu-apl-save-function (C-c C-c)")))

(defun gnu-apl--parse-function-header (string)
  "Parse a function definition string. Returns a list of four
elements. The values are: Result variable, left argument,
function name, right argument."
  (let ((line-fix (gnu-apl--trim "[ \t]" string)))
    (when (and (> (length line-fix) 0)
               (string= (char-to-string (aref line-fix 0)) "∇"))
      (let ((line (subseq line-fix 1)))
        (when (string-match (concat "^ *\\(?:\\([a-z0-9∆_]+\\) *← *\\)?" ; result variable
                                    "\\([a-za-z0-9∆_ ]+\\)" ; function and arguments
                                    "\\(;.*\\)?$" ; local variables
                                    )
                            line)
          (let ((result-variable (match-string 1 line))
                (function-and-arguments (match-string 2 line))
                (local-variables (match-string 3 line)))
            (let* ((parts (split-string function-and-arguments))
                   (length (length parts)))
              (when (and (>= length 1) (<= length 3))
                (append (list result-variable)
                        (ecase (length parts)
                          (1 (list nil (car parts) nil))
                          (2 (list nil (car parts) (cadr parts)))
                          (3 (list (car parts) (cadr parts) (caddr parts)))))))))))))

(defun gnu-apl-save-function ()
  "Save the currently edited function."
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
                          buffer-content
                        (concat buffer-content "\n"))))

        (gnu-apl-interactive-send-string (concat "'" *gnu-apl-ignore-start* "'\n"))
        (gnu-apl-interactive-send-string (concat ")ERASE " (caddr function-arguments)))
        (gnu-apl-interactive-send-string (concat "'" *gnu-apl-ignore-end* "'\n"))
        (gnu-apl-interactive-send-string (concat content "∇\n"))
        (let ((window-configuration (if (boundp 'gnu-apl-window-configuration)
                                        gnu-apl-window-configuration
                                      nil)))
          (kill-buffer (current-buffer))
          (when window-configuration
            (set-window-configuration window-configuration)))))))

(defun gnu-apl--send (proc string)
  (let* ((trimmed (gnu-apl--trim " " string))
         (parsed (gnu-apl--parse-function-header trimmed)))
    (if (and gnu-apl-auto-function-editor-popup parsed)
        (progn
          ;; At this point there should be a function definition symbol
          ;; at the beginning of the string. Let's confirm this:
          (unless (string= (subseq trimmed 0 1) "∇")
            (error "Unexpected format in function definition command"))
          (gnu-apl--get-function (gnu-apl--trim " " (subseq string 1))))
      (comint-simple-send proc string))))
