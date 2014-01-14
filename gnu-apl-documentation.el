;;; -*- lexical-binding: t -*-

;;;
;;; Keymap buffer
;;;

(defvar *gnu-apl-keymap-buffer-name* "*gnu-apl keymap*")

(defvar gnu-apl--ibm-copyright-notice
  "Reprint Courtesy of International Business Machines Corporation,
© 1984, 1994 International Business Machines Corporation"
  "Copyright notice that is appended to any documentation that is
dervived from the APL2 documentation.")

(defun gnu-apl-keymap-mode-kill-buffer ()
  (interactive)
  (let ((buffer (get-buffer *gnu-apl-keymap-buffer-name*)))
    (when buffer
      (delete-windows-on buffer)
      (kill-buffer buffer))))

(defvar gnu-apl-keymap-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'gnu-apl-keymap-mode-kill-buffer)
    map)
  "Keymap for keymap mode buffers")

(define-derived-mode gnu-apl-keymap-mode fundamental-mode "GNU APL Keymap"
  "Major mode for displaying the keymap help"
  (use-local-map gnu-apl-keymap-mode-map)
  (read-only-mode))

(defun gnu-apl--get-full-docstring-for-symbol (string)
  (let ((doc (cl-find string gnu-apl--symbol-doc :key #'car :test #'string=)))
    (when doc
      (with-output-to-string
        (loop for e in (second doc)
              for first = t then nil
              unless first
              do (princ "\n")
              do (progn
                   (princ (format "%s: %s\n%s\n\n" (first e) (second e) (third e)))
                   (let ((long (fourth e)))
                     (when long
                       (princ (format "%s\n" long)))))
              do (princ "\n===================================\n"))
        (when (third doc)
          (princ (format "\n%s" gnu-apl--ibm-copyright-notice)))))))

(defun gnu-apl-show-help-for-symbol-point ()
  "Open the help window for the symbol at point."
  (interactive)
  (let ((char (char-after (point))))
    (when char
      (gnu-apl-show-help-for-symbol (char-to-string char)))))

(defvar *gnu-apl-documentation-buffer-name* "*gnu-apl documentation*")

(defun gnu-apl-close-documentation-buffer ()
  "Closes the active documentation window"
  (interactive)
  (quit-window))

(defun gnu-apl-show-help-for-symbol (symbol)
  "Open the help window for SYMBOL."
  (interactive "MSymbol: ")
  (unless (= (length symbol) 1)
    (error "Symbol must be a single character"))
  (let ((string (gnu-apl--get-full-docstring-for-symbol symbol)))
    (unless string
      (user-error "No documentation available for %s" symbol))
    (let ((old-buffer (get-buffer *gnu-apl-documentation-buffer-name*)))
      (when old-buffer
        (kill-buffer old-buffer)))
    (let ((buffer (get-buffer-create *gnu-apl-documentation-buffer-name*)))
      (with-current-buffer buffer
        (delete-region (point-min) (point-max))
        (insert string)
        (goto-char (point-min))
        (read-only-mode 1)
        (local-set-key (kbd "q") 'gnu-apl-close-documentation-buffer))
      (pop-to-buffer buffer))))

(defun gnu-apl--make-clickable (string keymap)
  (propertize string
              'mouse-face 'highlight
              'help-echo (concat "mouse-1: Show documentation for " string "\n"
                                 "mouse-2: Insert " string " in GNU APL buffer")
              'gnu-apl-insert string
              'keymap keymap
              'follow-link t))

(defun gnu-apl-mouse-insert-from-keymap (event)
  "In the keymap buffer, insert the symbol that was clicked."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (unless (windowp window)
      (error "Can't find window"))
    (let ((string (with-current-buffer (window-buffer window)
                    (get-text-property pos 'gnu-apl-insert)))
          (session (gnu-apl--get-interactive-session)))
      (with-current-buffer session
        (insert string)))))

(defun gnu-apl-symbol-insert-from-keymap ()
  (interactive)
  (let ((string (get-text-property (point) 'gnu-apl-insert))
        (session (gnu-apl--get-interactive-session)))
    (with-current-buffer session
      (insert string))))

(defun gnu-apl--make-help-property-keymap ()
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'gnu-apl-mouse-insert-from-keymap)
    (define-key map (kbd "RET") 'gnu-apl-symbol-insert-from-keymap)
    map))

(defun gnu-apl--make-readable-keymap ()
  (let ((keymap-template "╔════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦═════════╗
║ ~∇ ║ !∇ ║ @∇ ║ #∇ ║ $∇ ║ %∇ ║ ^∇ ║ &∇ ║ *∇ ║ (∇ ║ )∇ ║ _∇ ║ +∇ ║         ║
║ `∇ ║ 1∇ ║ 2∇ ║ 3∇ ║ 4∇ ║ 5∇ ║ 6∇ ║ 7∇ ║ 8∇ ║ 9∇ ║ 0∇ ║ -∇ ║ =∇ ║ BACKSP  ║
╠════╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦══════╣
║       ║ Q∇ ║ W∇ ║ E∇ ║ R∇ ║ T∇ ║ Y∇ ║ U∇ ║ I∇ ║ O∇ ║ P∇ ║ {∇ ║ }∇ ║  |∇  ║
║  TAB  ║ q∇ ║ w∇ ║ e∇ ║ r∇ ║ t∇ ║ y∇ ║ u∇ ║ i∇ ║ o∇ ║ p∇ ║ [∇ ║ ]∇ ║  \\∇  ║
╠═══════╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩══════╣
║ (CAPS   ║ A∇ ║ S∇ ║ D∇ ║ F∇ ║ G∇ ║ H∇ ║ J∇ ║ K∇ ║ L∇ ║ :∇ ║ \"∇ ║         ║
║  LOCK)  ║ a∇ ║ s∇ ║ d∇ ║ f∇ ║ g∇ ║ h∇ ║ j∇ ║ k∇ ║ l∇ ║ ;∇ ║ '∇ ║ RETURN  ║
╠═════════╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═════════╣
║             ║ Z∇ ║ X∇ ║ C∇ ║ V∇ ║ B∇ ║ N∇ ║ M∇ ║ <∇ ║ >∇ ║ ?∇ ║          ║
║  SHIFT      ║ z∇ ║ x∇ ║ c∇ ║ v∇ ║ b∇ ║ n∇ ║ m∇ ║ ,∇ ║ .∇ ║ /∇ ║  SHIFT   ║
╚═════════════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩══════════╝"))
    ;; Ensure that the buffer is recreated
    (let ((old-buffer (get-buffer *gnu-apl-keymap-buffer-name*)))
      (when old-buffer
        (kill-buffer old-buffer)))
    ;; Recreate the buffer according to the active keymap.
    (let ((buffer (get-buffer-create *gnu-apl-keymap-buffer-name*))
          (keymap (gnu-apl--make-help-property-keymap)))
      (with-current-buffer buffer
        (delete-region (point-min) (point-max))
        (insert keymap-template)
        (goto-char (point-min))
        (while (search-forward-regexp "\\(.\\)∇" nil t)
          (let* ((key (match-string 1))
                 (found (cl-find key gnu-apl--symbols :key #'third :test #'equal))
                 (result-string (if found (gnu-apl--make-clickable (second found) keymap) " ")))
            (replace-match (concat key result-string) t t)))
        (add-text-properties (point-min) (point-max) (list 'face 'gnu-apl-default))
        (goto-char (point-min))
        (gnu-apl-keymap-mode))
      buffer)))

(defun gnu-apl-show-keyboard (&optional arg)
  "When arg is nil, toggle the display of the keyboard help. If
positive, always show the buffer, if negative close the buffer if
it is open."
  (interactive "P")
  (let ((keyboard-help (get-buffer *gnu-apl-keymap-buffer-name*)))
    (if (and keyboard-help (get-buffer-window keyboard-help))
        ;; The buffer is displayed. Maybe close it.
        (when (or (null arg) (minusp arg))
          (gnu-apl-keymap-mode-kill-buffer))
      ;; The buffer is not displayed, check if it's supposed to be displayed
      (when (or (null arg) (plusp arg))
        (let* ((buffer (or (when nil ; Make sure the buffer is always created
                             (get-buffer *gnu-apl-keymap-buffer-name*))
                           (gnu-apl--make-readable-keymap)))
               (window (split-window nil (- (with-current-buffer buffer
                                              (1+ (count-lines (point-min) (point-max))))))))
          (set-window-buffer window buffer))))))

(defvar gnu-apl--function-regexp
  (regexp-opt (mapcar #'car gnu-apl--symbol-doc)))

;;;
;;;  Eldoc integration
;;;

(defun gnu-apl--is-point-on-argument-value ()
  (save-excursion
    (if (> (point) (point-min))
        ;; There is stuff to the left of point, check what that stuff is
        (progn
          (backward-char 1)
          (loop while (and (> (point) (point-min))
                           (cl-find (char-after (point)) " \t"))
                do (backward-char 1))
          (let ((symbol (char-after (point))))
            (and (not (string-match gnu-apl--function-regexp (char-to-string symbol)))
                 (not (cl-find symbol " \t\n")))))
      ;; No stuff to the left of point, that means the function is monadic
      nil)))

(defun gnu-apl--eldoc-data ()
  (when (looking-at (concat "\\(" gnu-apl--function-regexp "\\)"))
    (let* ((symbol (match-string 1))
           (doc (cl-find symbol gnu-apl--symbol-doc :test #'equal :key #'car)))
      (unless doc
        (error "doc should not be null"))
      ;; We have a documentation entry. Now we need to figure out if the call
      ;; is monadic or dyadic. It can be done by searching backwards until we hit
      ;; a non-space character or the beginning of the line.
      (let ((p (cl-find (if (gnu-apl--is-point-on-argument-value) "Dyadic" "Monadic") (second doc)
                        :key #'car :test #'string=)))
        (when p
          (format "%s: %s: %s" (first p) (second p) (third p)))))))

;;;
;;;  Help search
;;;

(defvar *gnu-apl-apropos-symbol-buffer-name* "*gnu-apl apropos symbol*")

(defvar gnu-apl-documentation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'gnu-apl-apropos-kill-buffer)
    map)
  "Keymap for keymap mode buffers")

(define-derived-mode gnu-apl-documentation-mode fundamental-mode "GNU APL Documentation"
  "Major mode for displaying GNU APL documentation"
  (use-local-map gnu-apl-documentation-mode-map)
  (read-only-mode))

(defun gnu-apl-apropos-kill-buffer ()
  (interactive)
  (let ((buffer (get-buffer *gnu-apl-apropos-symbol-buffer-name*)))
    (when buffer
      (delete-windows-on buffer)
      (kill-buffer buffer))))

(defun gnu-apl--open-apropos-results (result)
  (let ((buffer (gnu-apl--open-new-buffer *gnu-apl-apropos-symbol-buffer-name*)))
    (with-current-buffer buffer
      (dolist (s result)
        (insert-button (cadr s)
                       'action #'(lambda (event) (gnu-apl-show-help-for-symbol (caar s)))
                       'follow-link t)
        (insert "\n"))
      (gnu-apl-documentation-mode)
      (read-only-mode 1))
    (pop-to-buffer buffer)))

(defun gnu-apl-apropos-symbol (regexp)
  (interactive "MApropos symbol: ")
  (let ((result (loop for doc-entry in gnu-apl--symbol-doc
                      append (loop for e in (second doc-entry)
                                   when (or (string-match regexp (second e))
                                            (string-match regexp (third e)))
                                   collect (list doc-entry
                                                 (format "%s: %s: %s: %s"
                                                         (first doc-entry) (first e) (second e) (third e)))))))
    (if result
        (gnu-apl--open-apropos-results result)
      (message "No match"))))
