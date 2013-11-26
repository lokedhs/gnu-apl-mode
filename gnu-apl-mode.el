;;; -*- lexical-binding: t -*-

(require 'cl)

;;;###autoload
(defcustom gnu-apl-executable "apl"
  "Where the GNU APL implementaion is located."
  :type 'string
  :group 'gnu-apl)

;;; Keymap based on the image available at: http://www.sudleyplace.com/APL/Keyboard.ahtml
(defvar gnu-apl--symbols '(;; Top row
                           ("diamond" "⋄" "`")
                           ("commabar" "⍪" "S-`")
                           ("diaeresis" "¨" "1")
                           ("identical-to" "≡" "S-1")
                           ("macron" "¯" "2")
                           ("not-identical-to" "≢" "S-2")
                           ("less-than" "<" "3")
                           ("del-stile" "⍒" "S-3")
                           ("less-than-or-equal-to" "≤" "4")
                           ("delta-stile" "⍋" "S-4")
                           ("equals" "=" "5")
                           ("circle-stile" "⌽" "S-5")
                           ("greater-than-or-equal-to" "≥" "6")
                           ("circle-backslash" "⍉" "S-6")
                           ("greater-than" ">" "7")
                           ("circled-minus" "⊖" "S-7")
                           ("not-equal-to" "≠" "8")
                           ("circle-star" "⍟" "S-8")
                           ("logical-or" "∨" "9")
                           ("down-caret-tilde" "⍱" "S-9")
                           ("logical-and" "∧" "0")
                           ("up-caret-tilde" "⍲" "S-0")
                           ("multiplication-sign" "×" "-")
                           ("quad-colon" "⍠" "S--")
                           ("division-sign" "÷" "=")
                           ("quad-divide" "⌹" "S-=")
                           ;; First row
                           ("iota" "⍳" "i")
                           ("iota-underbar" "⍸" "S-i")
                           ("epsilon" "ε" "e")
                           ("epsilon-underbar" "⍷" "S-e")
                           ("omega" "⍵" "w")
                           ("omega-underbar" "⍹" "S-w")
                           ("tilde" "~" "t")
                           ("tilde-diaeresis" "⍨" "S-t")
                           ("uparrow" "↑" "y")
                           ("downarrow" "↓" "u")
                           ("circle" "○" "o")
                           ("circle-diaeresis" "⍥" "S-o")
                           ("pi" "π" "p")
                           ("star-diaeresis" "⍣" "S-p")
                           ("leftarrow" "←" "[")
                           ("quote-quad" "⍞" "S-[")
                           ("rightarrow" "→" "]")
                           ("zilde" "⍬" "S-]")
                           ("right-tack" "⊢" "\\")
                           ("left-tack" "⊣" "S-\\")
                           ("rho" "⍴" "r")
                           ("root" "√" "S-r")))

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
        (when key-sequence
          (define-key map (kbd (concat prefix key-sequence)) (gnu-apl--make-key-command-sym (car command))))))
    map))

(defvar gnu-apl-mode-map
  (gnu-apl--make-mode-map "s-"))

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
