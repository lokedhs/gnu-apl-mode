;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'thingatpt)

(load "gnu-apl-util")

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

(defcustom gnu-apl-show-apl-welcome t
  "Choose if the GNU APL welcome screen should be displayed.
When non-nil, display the GNU APL welcome screen. When this value
is nil, the apl binary is called with the --silent flag."
  :type 'boolean
  :group 'gnu-apl)

(defcustom gnu-apl-show-tips-on-start t
  "When non-nil, show some help when starting a new APL session."
  :type 'boolean
  :group 'gnu-apl)

(defcustom gnu-apl-native-listener-port 0
  "The port number that the native listener should listen to.
If zero, randomly choose an available port.
If -1, request the use of Unix domain sockets."
  :type 'integer
  :group 'gnu-apl)

;;; This parameter is not customisable since there are very few cases
;;; where it would beed to be changed.
(defvar gnu-apl-native-communication t
  "Enable the use of the Emacs native library that is part of GNU
APL. This library provides a communications channel that
gnu-apl-mode can use to communicate with the APL interpreter.
Normally, this value should be set to t, as without it many
functions will not work. If this option is set to t, and the
library fails to load for some reason, the features will be
automatically disabled anyway.")

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
                           ("diamond" "⋄" "`")
                           ("tilde-diaeresis" "⍨" "~")
                           ("diaeresis" "¨" "1")
                           ("inverted-exclamation-mark" "¡" "!")
                           ("macron" "¯" "2")
                           ("not-identical-to" "≢" "@")
                           ("less-than" "<" "3")
                           ("pound-sign" "£" "#")
                           ("less-than-or-equal-to" "≤" "4")
                           ("left-shoe-stile" "⍧" "$")
                           ("equals" "=" "5")
                           ("greater-than-or-equal-to" "≥" "6")
                           ("greater-than" ">" "7")
                           ("not-equal-to" "≠" "8")
                           ("quad-backslash" "⍂" "*")
                           ("logical-or" "∨" "9")
                           ("down-caret-tilde" "⍱" "(")
                           ("logical-and" "∧" "0")
                           ("up-caret-tilde" "⍲" ")")
                           ("multiplication-sign" "×" "-")
                           ("identical-to" "≡" "_")
                           ("division-sign" "÷" "=")
                           ("quad-divide" "⌹" "+")

                           ;; First row
                           ("question-mark" "?" "q")
                           ("inverted-question-mark" "¿" "Q")
                           ("iota" "⍳" "i")
                           ("iota-underbar" "⍸" "I")
                           ("omega" "⍵" "w")
                           ("circle-stile" "⌽" "W")
                           ("epsilon" "∊" "e")
                           ("epsilon-underbar" "⍷" "E")
                           ("tilde" "∼" "t")
                           ("circle-backslash" "⍉" "T")
                           ("uparrow" "↑" "y")
                           ("yen-sign" "¥" "Y")
                           ("downarrow" "↓" "u")
                           ("circle" "○" "o")
                           ("circle-diaeresis" "⍥" "O")
                           ("star-operator" "⋆" "p")
                           ("circle-star" "⍟" "P")
                           ("leftarrow" "←" "[")
                           ("rightarrow" "→" "]")
                           ("zilde" "⍬" "}")
                           ("shoe-jot" "⍝" "\\")
                           ("backslash-bar" "⍀" "|")
                           ("rho" "⍴" "r")

                           ;; Second row
                           ("alpha" "⍺" "a")
                           ("circled-minus" "⊖" "A")
                           ("left-ceiling" "⌈" "s")
                           ("left-floor" "⌊" "d")
                           ("underscore" "_" "f")
                           ("del-tilde" "⍫" "F")
                           ("nabla" "∇" "g")
                           ("del-stile" "⍒" "G")
                           ("increment" "∆" "h")
                           ("delta-stile" "⍋" "H")
                           ("ring-operator" "∘" "j")
                           ("jot-diaeresis" "⍤" "J")
                           ("apostrophe" "'" "k")
                           ("quad-diamond" "⌺" "K")
                           ("quad" "⎕" "l")
                           ("quote-quad" "⍞" "L")
                           ("right-tack" "⊢" ";")
                           ("left-tack" "⊣" "'")

                           ;; Third row
                           ("subset-of" "⊂" "z")
                           ("superset-of" "⊃" "x")
                           ("intersection" "∩" "c")
                           ("union" "∪" "v")
                           ("up-tack" "⊥" "b")
                           ("down-tack-jot" "⍎" "B")
                           ("down-tack" "⊤" "n")
                           ("up-tack-jot" "⍕" "N")
                           ("divides" "|" "m")
                           ("i-beam" "⌶" "M")
                           ("squish-quad" "⌷" ",")
                           ("comma-bar" "⍪" "<")
                           ("delta-underbar" "⍙" ">")
                           ("slash-bar" "⌿" "?")

                           ;; Extras
                           ("pi" "π")
                           ("star-diaeresis" "⍣")
                           ("root" "√")
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
          (define-key map (kbd (concat prefix s)) (gnu-apl--make-key-command-sym (car command))))
        (define-key map (kbd "C-c C-k") 'gnu-apl-show-keyboard)
        (define-key map (kbd "C-c C-h") 'gnu-apl-show-help-for-symbol-point)
        (define-key map (kbd "C-c C-a") 'gnu-apl-apropos-symbol)
        (define-key map (kbd "M-.") 'gnu-apl-find-function-at-point)
        (define-key map [menu-bar gnu-apl] (cons "APL" (make-sparse-keymap "APL")))
        (define-key map [menu-bar gnu-apl toggle-keyboard] '("Toggle keyboard" . gnu-apl-show-keyboard))
        (define-key map [menu-bar gnu-apl show-help-for-symbol] '("Documentation for symbol" . gnu-apl-show-help-for-symbol))
        (define-key map [menu-bar gnu-apl apropos-symbol] '("Search symbols" . gnu-apl-apropos-symbol))
        (define-key map [menu-bar gnu-apl find-symbol-at-point] '("Find symbol at point" . gnu-apl-find-function-at-point))))
    map))

(defvar gnu-apl-mode-map
  (let ((map (gnu-apl--make-mode-map "s-")))
    (define-key map (kbd "C-c r") 'gnu-apl-interactive-send-region)
    (define-key map (kbd "C-c C-c") 'gnu-apl-interactive-send-current-function)
    map))

(defvar gnu-apl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (loop for s in gnu-apl--symbols
          for char = (second s)
          when char
          do (modify-syntax-entry (aref char 0) "." table))
    (modify-syntax-entry (aref "⍝" 0) "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for gnu-apl-mode")

(defun gnu-apl--init-mode-common ()
  (set (make-local-variable 'eldoc-documentation-function) 'gnu-apl--eldoc-data)
  ;; TODO: It's an open question as to whether the below is a good idea
  ;; or if a user should manually set this from the hook
  ;;(setq buffer-face-mode-face 'gnu-apl-default)
  ;;(buffer-face-mode)
  )

(define-derived-mode gnu-apl-mode prog-mode "GNU APL"
  "Major mode for editing GNU APL files."
  :syntax-table gnu-apl-mode-syntax-table
  :group 'gnu-apl
  (use-local-map gnu-apl-mode-map)
  (gnu-apl--init-mode-common))

;;;
;;;  Load the other source files
;;;

(load "gnu-apl-input")
(load "gnu-apl-interactive")
(load "gnu-apl-editor")
(load "gnu-apl-network")
(load "gnu-apl-spreadsheet")
(load "gnu-apl-plot")
(if gnu-apl-use-free-documentation
    (load "gnu-apl-refdocs-bsd-license")
  (load "gnu-apl-refdocs-apl2"))
(load "gnu-apl-documentation")
(load "gnu-apl-osx-workaround")

(add-to-list 'auto-mode-alist '("\\.apl\\'" . gnu-apl-mode))

(provide 'gnu-apl-mode)
