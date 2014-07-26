;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'thingatpt)
(require 'comint)

(load "gnu-apl-util")

;;;###autoload
(defgroup gnu-apl nil
  "Major mode for interacting with the GNU APL interpreter."
  :prefix 'gnu-apl
  :group 'languages)

;;;###autoload
(defcustom gnu-apl-executable "apl"
  "Where the GNU APL implementaion is located."
  :type 'string
  :group 'gnu-apl)

;;;###autoload
(defcustom gnu-apl-auto-function-editor-popup t
  "Edit function definitions in an Emacs buffer.
If non-nil, the function editor will start automatically when
the function definition command is entered. If nil, the
function editor must be opened manually using the function
`gnu-apl-edit-function'.")

;;;###autoload
(defcustom gnu-apl-redefine-function-when-in-use-action 'ask
  "What action to take when trying to save a function that is on the )SI stack.
This parameter controls the behaviour when an attempt is made
to redefine a function which is already on the )SI stack.
Permitted values are:

    error - Signal an error message
    clear - Clear the )SI stack before editing
    ask - Ask the user what action to take"
  :type '(choice (const :tag "error" error)
                 (const :tag "clear" clear)
                 (const :tag "ask" ask))
  :group 'gnu-apl)

;;;###autoload
(defcustom gnu-apl-show-keymap-on-startup t
  "Choose if the keymap should be automatically displayed.
When non-nil, automatically display the keymap when activating
the GNU APL buffer using the command `gnu-apl'. The keyboard help
buffer can also be toggled using the command
`gnu-apl-show-keyboard'."
  :type 'boolean
  :group 'gnu-apl)

;;;###autoload
(defcustom gnu-apl-show-apl-welcome t
  "Choose if the GNU APL welcome screen should be displayed.
When non-nil, display the GNU APL welcome screen. When this value
is nil, the apl binary is called with the --silent flag."
  :type 'boolean
  :group 'gnu-apl)

;;;###autoload
(defcustom gnu-apl-show-tips-on-start t
  "When non-nil, show some help when starting a new APL session."
  :type 'boolean
  :group 'gnu-apl)

;;;###autoload
(defcustom gnu-apl-native-listener-port 0
  "The port number that the native listener should listen to.
If zero, randomly choose an available port.
If -1, request the use of Unix domain sockets."
  :type 'integer
  :group 'gnu-apl)

;;;###autoload
(defcustom gnu-apl-gnuplot-program "gnuplot"
  "The name of the gnuplot executable."
  :type 'string
  :group 'gnu-apl)

;;; This parameter is not customisable since there are very few cases
;;; where it would need to be changed.
(defvar gnu-apl-native-communication t
  "Enable the use of the Emacs native library that is part of GNU
APL. This library provides a communications channel that
gnu-apl-mode can use to communicate with the APL interpreter.
Normally, this value should be set to t, as without it many
functions will not work. If this option is set to t, and the
library fails to load for some reason, the features will be
automatically disabled anyway.")

;;;###autoload
(defcustom gnu-apl-program-extra-args nil
  "List of strings containing extra commandline arguments to pass
  to the apl binary."
  :type '(repeat string)
  :group 'gnu-apl)

(defvar gnu-apl-use-new-native-library nil
  "If non-nil, use the new-style native library.
Enabling this option requires the use of at least GNU APL version 1.4
or the latest version from the subversion repository.")

;;;###autoload
(defface gnu-apl-default
  ()
  "Face used for APL buffers"
  :group 'gnu-apl)

;;;###autoload
(defface gnu-apl-error
  '((((class color))
     :foreground "red"
     :inherit gnu-apl-default)
    (t
     :inherit gnu-apl-default))
  "Face used for error messages in the interactive APL buffer"
  :group 'gnu-apl)

;;;###autoload
(defface gnu-apl-user-status-text
  '((((class color))
     :foreground "#ff0080"
     :inherit gnu-apl-default)
    (t
     :inherit gnu-apl-default))
  "Face used for user diagnostic messages in the interactive APL buffer"
  :group 'gnu-apl)

;;;###autoload
(defface gnu-apl-help
  '((t
     :inherit gnu-apl-default))
  "Face used for displaying text in help buffers"
  :group 'gnu-apl)

;;;###autoload
(defface gnu-apl-kbd-help-screen
  '((t
     :inherit gnu-apl-default))
  "Face used to display the keyboard help popup"
  :group 'gnu-apl)

(defvar gnu-apl-use-free-documentation nil
  "If this value is set to true prior to loading, the non-free
documentation will not be loaded.")

;;; ' ( ) + , - . /  :  ; < = >  ? [ ]
;;; \ _ ¨ ¯ × ÷ ← ↑ → ↓ ∆ ∇ ∘ ∣ ∧ ∨
;;; ∩ ∪ ∼ ≠ ≤ ≥ ≬ ⊂ ⊃ ⌈ ⌊ ⊤ ⊥ ⋆ ⌶ ⌷
;;; ⌸ ⌹ ⌺ ⌻ ⌼ ⌽ ⌾ ⌿ ⍀ ⍁ ⍂ ⍃ ⍄ ⍅ ⍆ ⍇
;;; ⍈ ⍉ ⍊ ⍋ ⍌ ⍍ ⍎ ⍏ ⍐ ⍑ ⍒ ⍓ ⍔ ⍕ ⍖ ⍗
;;; ⍘ ⍙ ⍚ ⍛ ⍜ ⍝ ⍞ ⍟ ⍠ ⍡ ⍢ ⍣ ⍤ ⍥ ⍦ ⍧
;;; ⍨ ⍩ ⍪ ⍫ ⍬ ⍭ ⍮ ⍯ ⍰ ⍱ ⍲ ⍳ ⍴ ⍵ ⍶ ⍷
;;; ⍸ ⍹ ⍺ ⎕ ○

;;; Keymap based on the image available at: http://www.sudleyplace.com/APL/Keyboard.ahtml
;;; GNU APL keyboard layout: http://commons.wikimedia.org/wiki/File:GNU_APL_keyboard_layout.png
(defvar gnu-apl--symbols '(;; Top row
                           ;; `
                           ("diamond" "◊" "`")
                           ;; 1
                           ("diaeresis" "¨" "1")
                           ("i-beam" "⌶" "!")
                           ;; 2
                           ("macron" "¯" "2")
                           ("del-tilde" "⍫" "@")
                           ;; 3
                           ("less-than" "<" "3")
                           ("del-stile" "⍒" "#")
                           ;; 4
                           ("less-than-or-equal-to" "≤" "4")
                           ("delta-stile" "⍋" "$")
                           ;; 5
                           ("equals" "=" "5")
                           ("circle-stile" "⌽" "%")
                           ;; 6
                           ("greater-than-or-equal-to" "≥" "6")
                           ("circle-backslash" "⍉" "^")
                           ;; 7
                           ("greater-than" ">" "7")
                           ("circled-minus" "⊖" "&")
                           ;; 8
                           ("not-equal-to" "≠" "8")
                           ("circle-star" "⍟" "*")
                           ;; 9
                           ("logical-or" "∨" "9")
                           ("down-caret-tilde" "⍱" "(")
                           ;; 0
                           ("logical-and" "∧" "0")
                           ("up-caret-tilde" "⍲" ")")
                           ;; -
                           ("multiplication-sign" "×" "-")
                           ("exclamation-mark" "!" "_")
                           ;; =
                           ("division-sign" "÷" "=")
                           ("quad-divide" "⌹" "+")

                           ;; First row
                           ;; q
                           ("question-mark" "?" "q")
                           ;; w
                           ("omega" "⍵" "w")
                           ("omega-underbar" "⍹" "W")
                           ;; e
                           ("epsilon" "∊" "e")
                           ("epsilon-underbar" "⍷" "E")
                           ;; r
                           ("rho" "⍴" "r")
                           ;; t
                           ("tilde" "∼" "t")
                           ("tilde-diaeresis" "⍨" "T")
                           ;; y
                           ("uparrow" "↑" "y")
                           ("yen-sign" "¥" "Y")
                           ;; u
                           ("downarrow" "↓" "u")
                           ;; i
                           ("iota" "⍳" "i")
                           ("iota-underbar" "⍸" "I")
                           ;; o
                           ("circle" "○" "o")
                           ("circle-diaeresis" "⍥" "O")
                           ;; p
                           ("star-operator" "⋆" "p")
                           ("star-diaeresis" "⍣" "P")
                           ;; [
                           ("leftarrow" "←" "[")
                           ("quote-quad" "⍞" "{")
                           ;; ]
                           ("rightarrow" "→" "]")
                           ("zilde" "⍬" "}")
                           ;; \
                           ("right-tack" "⊢" "\\")
                           ("left-tack" "⊣" "|")

                           ;; Second row
                           ;; a
                           ("alpha" "⍺" "a")
                           ("alpha-underbar" "⍶" "A")
                           ;; s
                           ("left-ceiling" "⌈" "s")
                           ;; d
                           ("left-floor" "⌊" "d")
                           ;; f
                           ("underscore" "_" "f")
                           ("del-tilde" "⍫" "F")
                           ;; g
                           ("nabla" "∇" "g")
                           ;; h
                           ("increment" "∆" "h")
                           ("delta-underbar" "⍙" "H")
                           ;; j
                           ("ring-operator" "∘" "j")
                           ("jot-diaeresis" "⍤" "J")
                           ;; k
                           ("apostrophe" "'" "k")
                           ("quad-diamond" "⌺" "K")
                           ;; l
                           ("quad" "⎕" "l")
                           ("squish-quad" "⌷" "L")
                           ;; ;
                           ("down-tack-jot" "⍎" ";")
                           ("identical-to" "≡" ":")
                           ;; '
                           ("up-tack-jot" "⍕" "'")
                           ("not-identical-to" "≢" "\"")

                           ;; Third row
                           ;; z
                           ("subset-of" "⊂" "z")
                           ;; x
                           ("superset-of" "⊃" "x")
                           ("greek-letter-chi" "χ" "X")
                           ;; c
                           ("intersection" "∩" "c")
                           ("left-shoe-stile" "⍧" "C")
                           ;; v
                           ("union" "∪" "v")
                           ;; b
                           ("up-tack" "⊥" "b")
                           ("pound-sign" "£" "B")
                           ;; n
                           ("down-tack" "⊤" "n")
                           ;; m
                           ("divides" "|" "m")
                           ;; ,
                           ("shoe-jot" "⍝" ",")
                           ("comma-bar" "⍪" "<")
                           ;; .
                           ("backslash-bar" "⍀" ">")
                           ;; /
                           ("slash-bar" "⌿" "/")
                           ("quad-colon" "⍠" "?")
                           
                           ;; Extras
                           ("pi" "π")
                           ("root" "√")
                           ("inverted-exclamation-mark" "¡")
                           ("quad-backslash" "⍂")
                           ("inverted-question-mark" "¿")
                           ))

(defun gnu-apl--make-key-command-sym (n)
  (intern (concat "insert-sym-apl-" n)))

(macrolet ((make-insert-functions ()
             `(progn
                ,@(mapcar #'(lambda (command)
                              `(defun ,(gnu-apl--make-key-command-sym (car command)) ()
                                 (interactive)
                                 (insert-string ,(cadr command))))
                          gnu-apl--symbols))))
  (make-insert-functions))

(defun gnu-apl--make-mode-map (prefix)
  (let ((map (make-sparse-keymap)))
    (dolist (command gnu-apl--symbols)
      (let ((key-sequence (caddr command)))
        (dolist (s (if (listp key-sequence) key-sequence (list key-sequence)))
          (define-key map (gnu-apl--kbd (concat prefix s)) (gnu-apl--make-key-command-sym (car command))))
        (define-key map (kbd "C-c C-k") 'gnu-apl-show-keyboard)
        (define-key map (kbd "C-c C-h") 'gnu-apl-show-help-for-symbol)
        (define-key map (kbd "C-c C-a") 'gnu-apl-apropos-symbol)
        (define-key map (kbd "M-.") 'gnu-apl-find-function-at-point)
        (define-key map (kbd "C-c C-.") 'gnu-apl-trace)
        (define-key map [menu-bar gnu-apl] (cons "APL" (make-sparse-keymap "APL")))
        (define-key map [menu-bar gnu-apl toggle-keyboard] '("Toggle keyboard" . gnu-apl-show-keyboard))
        (define-key map [menu-bar gnu-apl show-help-for-symbol] '("Documentation for symbol" . gnu-apl-show-help-for-symbol))
        (define-key map [menu-bar gnu-apl apropos-symbol] '("Search symbols" . gnu-apl-apropos-symbol))
        (define-key map [menu-bar gnu-apl find-symbol-at-point] '("Find symbol at point" . gnu-apl-find-function-at-point))
        (define-key map [menu-bar gnu-apl trace] '("Trace variable" . gnu-apl-trace))))
    map))

(defvar gnu-apl-mode-map
  (let ((map (gnu-apl--make-mode-map "s-")))
    (define-key map (kbd "C-c r") 'gnu-apl-interactive-send-region)
    (define-key map (kbd "C-c C-c") 'gnu-apl-interactive-send-current-function)
    map)
  "The keymap for gnu-apl-mode")

(defvar gnu-apl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (loop for s in gnu-apl--symbols
          for char = (second s)
          when char
          do (modify-syntax-entry (aref char 0) "." table))
    (modify-syntax-entry (aref "⍝" 0) "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry (aref "∆" 0) "w" table)
    (modify-syntax-entry (aref "⍙" 0) "w" table)
    (modify-syntax-entry ?\\ "." table)
    table)
  "Syntax table for gnu-apl-mode")

(defun gnu-apl--init-mode-common ()
  "Genering initialisation code for all gnu-apl modes"
  (set (make-local-variable 'eldoc-documentation-function) 'gnu-apl--eldoc-data)
  (set (make-local-variable 'completion-at-point-functions) '(gnu-apl-expand-symbol))
  (set (make-local-variable 'tab-always-indent) 'complete)
  (set (make-local-variable 'indent-line-function) 'gnu-apl-indent)
  (set (make-local-variable 'comment-start) "⍝")
  (set (make-local-variable 'comment-padding) " ")
  (set (make-local-variable 'comment-end) "")
  ;; TODO: It's an open question as to whether the below is a good idea
  ;; or if a user should manually set this from the hook
  ;;(setq buffer-face-mode-face 'gnu-apl-default)
  ;;(buffer-face-mode)
  )

(defvar gnu-apl-font-lock-keywords1
  '("⎕[a-zA-Z0-9]+"))

(defvar gnu-apl--apl-symbol-pattern "[a-zA-Z_⍺⍵⍶⍹∆⍙λ][a-zA-Z0-9_⍺⍵⍶⍹∆⍙λ¯]*")

(defvar gnu-apl--function-declaration-patterns
  (let* ((s gnu-apl--apl-symbol-pattern)
         (f (format "\\(?: *\\[ *%s *\\]\\)?" s)))

    ;; Patterns that cover the following variations:
    ;;    FN
    ;;    FN R
    ;;    L FN R
    ;;    (LO FN)
    ;;    (LO FN) R
    ;;    (LO FN RO)
    ;;    (LO FN RO) R
    ;;    L (LO FN) R
    ;;    L (LO FN RO) R
    (labels ((add-assignment-syntax (regexp) (concat (format "\\(?:%s *← *\\)?" s)
                                                     regexp
                                                     " *\\(?:;.*\\)?$")))
      (list (add-assignment-syntax (format "\\(%s\\)" s))
            (add-assignment-syntax (format "\\(?:%s +\\)?\\(%s\\)%s +%s" s s f s))
            (add-assignment-syntax (format "( *%s +\\(%s\\) *)%s" s s f))
            (add-assignment-syntax (format "\\(?:%s +\\)?( *%s +\\(%s\\) *)%s +%s" s s s f s))
            (add-assignment-syntax (format "( *%s +\\(%s\\) +%s)%s" s s s f))
            (add-assignment-syntax (format "\\(?:%s +\\)?( *%s +\\(%s\\) +%s)%s +%s" s s s s f s))))))

(defun gnu-apl--match-function-head (limit)
  (loop for pattern in gnu-apl--function-declaration-patterns
        for result = (search-forward-regexp (format "^∇ *%s" pattern) limit t)
        when result
        return t
        finally (return nil)))

(defun gnu-apl--parse-function-header (string)
  "Parse a function definition string. Returns the name of the
function or nil if the function could not be parsed."
  (let* ((line (gnu-apl--trim-spaces string)))
    (loop for pattern in gnu-apl--function-declaration-patterns
          when (string-match (concat "^" pattern) line)
          return (match-string 1 line))))

(define-derived-mode gnu-apl-mode prog-mode "GNU APL"
  "Major mode for editing GNU APL files."
  :syntax-table gnu-apl-mode-syntax-table
  :group 'gnu-apl
  (use-local-map gnu-apl-mode-map)
  (gnu-apl--init-mode-common)
  (gnu-apl--set-imenu-pattern)
  (set (make-local-variable 'font-lock-defaults)
       '((("⎕[a-zA-Z0-9]+" . font-lock-keyword-face)
          ("^[ \t]*[a-zA-Z_∆⍙λ⍺⍵][a-zA-Z0-9_∆⍙λ⍺⍵¯]+:" . font-lock-builtin-face)
          (gnu-apl--match-function-head . (1 font-lock-function-name-face)))
         nil nil nil)))

(defun gnu-apl--symbol-at-point ()
  (let ((symbol (thing-at-point 'symbol)))
    symbol))

(defun gnu-apl--find-largest-backward-match (regex)
  (save-excursion
    (loop with old-pos = nil
          for pos = (save-excursion (search-backward-regexp regex nil t))
          while pos
          do (progn
               (backward-char 1)
               (setq old-pos pos))
          finally (return old-pos))))

;;;
;;;  Indentation support
;;;

(defun gnu-apl--indent-safely (pos)
  (indent-line-to (max pos 0)))

(defun gnu-apl--full-function-definition-p (line &optional error-on-incorrect-format)
  (when (and (plusp (length line))
             (string= (subseq line 0 1) "∇"))
    (let ((parsed (gnu-apl--parse-function-header (subseq line 1))))
      (when (and error-on-incorrect-format
                 (null parsed))
        (user-error "Incorrectly formatted function header"))
      parsed)))

(defun gnu-apl--indent-this ()
  ;; The indentation rules are very simple. If we are in a function,
  ;; indent by 2 characters unless we are on a label definition in
  ;; which case the line should not be indented.
  ;; Anything outside a function definition is not indented.
  (beginning-of-line)
  (save-excursion
    (when (re-search-forward "\\=[ \t]*" nil t)
      (replace-match "" nil nil)))
  (cond ((looking-at "∇")
         (indent-to-column 0))
        ((looking-at (format "%s:" gnu-apl--apl-symbol-pattern))
         (indent-to-column 0))
        (t
         (let ((funtion-start (save-excursion
                                (search-backward-regexp "^[ \t]*∇[ \t]*[^ \t]" nil t))))
           (if (not funtion-start)
               (indent-to-column 0)
             (let ((function-end (save-excursion
                                   (search-backward-regexp "^[ \t]*∇[ \t]$" nil t))))
               (if (or (not function-end)
                       (< function-start function-end))
                   (indent-to-column 2)
                 (indent-to-column 0)))))))
  nil)

(defun gnu-apl-indent ()
  ;; No indentation unless the cursor is at the beginning of the line
  (if (save-excursion (search-backward-regexp "^[ \t]*\\=" nil t))
      (gnu-apl--indent-this)
    'noindent))

(defun gnu-apl--load-commands (prefix)
  (let ((results (gnu-apl--send-network-command-and-read "systemcommands")))
    (cl-remove-if-not #'(lambda (v)
                          (gnu-apl--string-match-start v prefix))
                      results)))

;;;
;;;  Support for expansion
;;;

(defun gnu-apl-expand-symbol ()
  (let* ((row (buffer-substring (save-excursion (beginning-of-line) (point)) (point))))
    ;; Check for system commands
    (if (string-match "^[ \t]*\\([])][a-zA-Z0-9]*\\)$" row)
        (let* ((cmdname (match-string 1 row))
               (command-start-index (- (point) (length cmdname))))
          (list command-start-index (point) (gnu-apl--load-commands cmdname)))

      ;; Check for quad-commands
      (let ((svar-pos (gnu-apl--find-largest-backward-match "⎕[a-zA-Z0-9]*\\=")))
        (if svar-pos
            (let* ((svar (buffer-substring svar-pos (point)))
                   (results (gnu-apl--send-network-command-and-read "systemvariables"))
                   (filtered-variables (cl-remove-if-not #'(lambda (v)
                                                             (gnu-apl--string-match-start v svar))
                                                         results)))
              (when filtered-variables
                (list svar-pos (point) filtered-variables)))

          ;; Check for user-defined symbols
          (let ((pos (gnu-apl--find-largest-backward-match "[a-zA-Z_∆⍙][a-zA-Z0-9_∆⍙¯]*\\=")))
            (when pos
              (let* ((s (buffer-substring pos (point)))
                     (results (gnu-apl--send-network-command-and-read "variables"))
                     (filtered-variables (cl-remove-if-not #'(lambda (v)
                                                               (gnu-apl--string-match-start v s))
                                                           results)))
                (when filtered-variables
                  (list pos (point) filtered-variables))))))))))

;;;
;;;  imenu integration
;;;

(defun gnu-apl--set-imenu-pattern ()
  (setq imenu-generic-expression
        (mapcar #'(lambda (v) (list nil (concat "^∇ *" v) 1))
                gnu-apl--function-declaration-patterns)))

;;;
;;;  Load the other source files
;;;

(load "gnu-apl-input")
(load "gnu-apl-interactive")
(load "gnu-apl-editor")
(load "gnu-apl-network")
(load "gnu-apl-spreadsheet")
(load "gnu-apl-plot")
(load "gnu-apl-follow")
(load "gnu-apl-refdocs-bsd-license")
(load "gnu-apl-documentation")
(load "gnu-apl-osx-workaround")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.apl\\'" . gnu-apl-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("gnu-apl" . gnu-apl-mode))

(provide 'gnu-apl-mode)
