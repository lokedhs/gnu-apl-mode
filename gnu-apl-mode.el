;;; -*- lexical-binding: t -*-

(require 'cl)

;;;###autoload
(defcustom gnu-apl-executable "apl"
  "Where the GNU APL implementaion is located."
  :type 'string
  :group 'gnu-apl)

;;;###autoload
(defface gnu-apl-default
  ()
  "Face used for APL buffers"
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
                           ("diamond" "⋄" "`" "`")
                           ("tilde-diaeresis" "⍨" "S-`")
                           ("diaeresis" "¨" "1" "1")
                           ("identical-to" "≡" "S-1" "!")
                           ("macron" "¯" "2" "2")
                           ("not-identical-to" "≢" "S-2" "@")
                           ("less-than" "<" "3" "3")
                           ("pound-sign" "£" "S-3" "#")
                           ("less-than-or-equal-to" "≤" "4" "4")
                           ("left-shoe-stile" "⍧" "S-4" "$")
                           ("equals" "=" "5" "5")
                           ("greater-than-or-equal-to" "≥" "6" "6")
                           ("greater-than" ">" "7" "7")
                           ("not-equal-to" "≠" "8" "8")
                           ("quad-backslash" "⍂" "S-8" "*")
                           ("logical-or" "∨" "9" "9")
                           ("down-caret-tilde" "⍱" "S-9" "(")
                           ("logical-and" "∧" "0" "0")
                           ("up-caret-tilde" "⍲" "S-0" ")")
                           ("multiplication-sign" "×" "-" "-")
                           ("identical-to" "≡" "S--" "_")
                           ("division-sign" "÷" "=" "=")
                           ("quad-divide" "⌹" "S-=" "+")

                           ;; First row
                           ("question-mark" "?" "q" "q")
                           ("inverted-question-mark" "¿" "S-q" "Q")
                           ("iota" "⍳" "i" "i")
                           ("iota-underbar" "⍸" "S-i" "I")
                           ("omega" "⍵" "w" "w")
                           ("circle-stile" "⌽" "S-w" "W")
                           ("epsilon" "ε" "e" "e")
                           ("epsilon-underbar" "⍷" "S-e" "E")
                           ("tilde" "~" "t" "t")
                           ("circle-backslash" "⍉" "S-t" "T")
                           ("uparrow" "↑" "y" "y")
                           ("yen-sign" "¥" "S-y" "Y")
                           ("downarrow" "↓" "u" "u")
                           ("circle" "○" "o" "o")
                           ("circle-diaeresis" "⍥" "S-o" "O")
                           ("star-operator" "⋆" "p" "p")
                           ("circle-star" "⍟" "S-p" "P")
                           ("leftarrow" "←" "[" "[")
                           ("rightarrow" "→" "]" "]")
                           ("shoe-jot" "⍝" ("\\" "S-c") ("\\" "C"))
                           ("backslash-bar" "⍀" "S-\\" "|")
                           ("rho" "⍴" "r" "r")
                           ("root" "√" "S-r" "R")

                           ;; Second row
                           ("alpha" "⍺" "a" "a")
                           ("zilde" "⍬" "S-a" "A")
                           ("left-ceiling" "⌈" "s" "s")
                           ("left-floor" "⌊" "d" "d")
                           ("underscore" "_" "f" "f")
                           ("del-tilde" "⍫" "S-f" "F")
                           ("nabla" "∇" "g" "g")
                           ("del-stile" "⍒" "S-g" "G")
                           ("increment" "∆" ("h" "S-.") ("h" ">"))
                           ("delta-stile" "⍋" "S-h" "H")
                           ("ring-operator" "∘" "j" "j")
                           ("jot-diaeresis" "⍤" "S-j" "J")
                           ("apostrophe" "'" "k" "k")
                           ("quad-diamond" "⌺" "S-k" "K")
                           ("quad" "⎕" "l" "l")
                           ("quote-quad" "⍞" "S-l" "L")
                           ("right-tack" "⊢" ";" ":")
                           ("left-tack" "⊣" "'" "\"")

                           ;; Third row
                           ("subset-of" "⊂" "z" "z")
                           ("superset-of" "⊃" "x" "x")
                           ("intersection" "∩" "c" "c")
                           ("union" "∪" "v" "v")
                           ("up-tack" "⊥" "b" "b")
                           ("down-tack-jot" "⍎" ("S-b" ".") ("B" "."))
                           ("down-tack" "⊤" "n" "n")
                           ("up-tack-jot" "⍕" ("S-n" "/") ("N" "/"))
                           ("divides" "∣" "m" "m")
                           ("i-beam" "⌶" "S-m" "M")
                           ("squish-quad" "⌷" "," ",")
                           ("comma-bar" "⍪" "S-," "<")
                           ("slash-bar" "⌿" "/" "/")

                           ;; Extras
                           ("pi" "π")
                           ("star-diaeresis" "⍣")
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
          (define-key map (kbd (concat prefix s)) (gnu-apl--make-key-command-sym (car command))))))
    map))

(defvar gnu-apl-mode-map
  (gnu-apl--make-mode-map "s-"))

(load "gnu-apl-input")

(define-derived-mode gnu-apl-mode comint-mode "GNU APL"
  "Major mode for interacting with GNU APL."
  (use-local-map gnu-apl-mode-map)
  (setq comint-prompt-regexp "^      ")
  (setq buffer-face-mode-face 'gnu-apl-default)
  (buffer-face-mode))

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
