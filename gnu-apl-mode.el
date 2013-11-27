;;; -*- lexical-binding: t -*-

(require 'cl)

;;;###autoload
(defcustom gnu-apl-executable "apl"
  "Where the GNU APL implementaion is located."
  :type 'string
  :group 'gnu-apl)

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
                           ;;("commabar" "⍪" "S-`")
                           ("tilde-diaeresis" "⍨" "S-`")
                           ("diaeresis" "¨" "1")
                           ("identical-to" "≡" "S-1")
                           ("macron" "¯" "2")
                           ("not-identical-to" "≢" "S-2")
                           ("less-than" "<" "3")
                           ;;("del-stile" "⍒" "S-3")
                           ("pound-sign" "£" "S-3")
                           ("less-than-or-equal-to" "≤" "4")
                           ;;("delta-stile" "⍋" "S-4")
                           ("left-shoe-stile" "⍧" "S-4")
                           ("equals" "=" "5")
                           ;;("circle-stile" "⌽" "S-5")
                           ("greater-than-or-equal-to" "≥" "6")
                           ;;("circle-backslash" "⍉" "S-6")
                           ("greater-than" ">" "7")
                           ;;("circled-minus" "⊖" "S-7")
                           ("not-equal-to" "≠" "8")
                           ;;("circle-star" "⍟" "S-8")
                           ("quad-backslash" "⍂" "S-8")
                           ("logical-or" "∨" "9")
                           ("down-caret-tilde" "⍱" "S-9")
                           ("logical-and" "∧" "0")
                           ("up-caret-tilde" "⍲" "S-0")
                           ("multiplication-sign" "×" "-")
                           ;;("quad-colon" "⍠" "S--")
                           ("identical-to" "≡" "S--")
                           ("division-sign" "÷" "=")
                           ("quad-divide" "⌹" "S-=")

                           ;; First row
                           ("question-mark" "?" "q")
                           ("inverted-question-mark" "¿" "S-q")
                           ("iota" "⍳" "i")
                           ("iota-underbar" "⍸" "S-i")
                           ("omega" "⍵" "w")
                           ;;("omega-underbar" "⍹" "S-w")
                           ("circle-stile" "⌽" "S-w")
                           ("epsilon" "ε" "e")
                           ("epsilon-underbar" "⍷" "S-e")
                           ("tilde" "~" "t")
                           ;;("tilde-diaeresis" "⍨" "S-t")
                           ("circle-backslash" "⍉" "S-t")
                           ("uparrow" "↑" "y")
                           ("yen-sign" "¥" "S-y")
                           ("downarrow" "↓" "u")
                           ("circle" "○" "o")
                           ("circle-diaeresis" "⍥" "S-o")
                           ;;("pi" "π" "p")
                           ;;("star-diaeresis" "⍣" "S-p")
                           ("star-operator" "⋆" "p")
                           ("circle-star" "⍟" "S-p")
                           ("leftarrow" "←" "[")
                           ;;("quote-quad" "⍞" "S-[")
                           ("rightarrow" "→" "]")
                           ;;("zilde" "⍬" "S-]")
                           ;;("right-tack" "⊢" "\\")
                           ;;("left-tack" "⊣" "S-\\")
                           ("shoe-jot" "⍝" ("\\" "S-c"))
                           ("backslash-bar" "⍀" "S-\\")
                           ("rho" "⍴" "r")
                           ("root" "√" "S-r")

                           ;; Second row
                           ("alpha" "⍺" "a")
                           ("zilde" "⍬" "S-a")
                           ("left-ceiling" "⌈" "s")
                           ("left-floor" "⌊" "d")
                           ("underscore" "_" "f")
                           ("del-tilde" "⍫" "S-f")
                           ("nabla" "∇" "g")
                           ("del-stile" "⍒" "S-g")
                           ("increment" "∆" ("h" "S-."))
                           ("delta-stile" "⍋" "S-h")
                           ("ring-operator" "∘" "j")
                           ("jot-diaeresis" "⍤" "S-j")
                           ("apostrophe" "'" "k")
                           ("quad-diamond" "⌺" "S-k")
                           ("quad" "⎕" "l")
                           ("quote-quad" "⍞" "S-l")
                           ("right-tack" "⊢" ";")
                           ("left-tack" "⊣" "'")

                           ;; Third row
                           ("subset-of" "⊂" "z")
                           ("superset-of" "⊃" "x")
                           ("intersection" "∩" "c")
                           ("union" "∪" "v")
                           ("up-tack" "⊥" "b")
                           ("down-tack-jot" "⍎" ("S-b" "."))
                           ("down-tack" "⊤" "n")
                           ("up-tack-jot" "⍕" ("S-n" "/"))
                           ("divides" "∣" "m")
                           ("i-beam" "⌶" "S-m")
                           ("squish-quad" "⌷" ",")
                           ("comma-bar" "⍪" "S-,")
                           ("slash-bar" "⌿" "/")))

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
          (define-key map (kbd (concat prefix s)) (gnu-apl--make-key-command-sym (car command))))))
    map))

(defvar gnu-apl-mode-map
  (gnu-apl--make-mode-map "s-"))

(load "gnu-apl-input")

(define-derived-mode gnu-apl-mode comint-mode "GNU APL"
  "Major mode for interacting with GNU APL."
  (use-local-map gnu-apl-mode-map)
  (setq comint-prompt-regexp "^      "))

(defun gnu-apl ()
  (interactive)
  (let ((buffer (get-buffer-create "*gnu-apl*")))
    (pop-to-buffer-same-window buffer)
    (unless (comint-check-proc buffer)
      (make-comint-in-buffer "apl" buffer gnu-apl-executable
                             nil
                             "--noCIN")
      (gnu-apl-mode))))

(provide 'gnu-apl-mode)
