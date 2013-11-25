;;; -*- lexical-binding: t -*-

(require 'cl)

(makunbound 'gnu-apl--symbols)
(defvar gnu-apl--symbols '(("iota" "⍳" "s-i")
                           ("omega" "⍵" "s-w")
                           ("omega-underbar" "⍹")
                           ("leftarrow" "←" "s-[")
                           ("rightarrow" "→" "s-]")
                           ("rho" "⍴")))

(defun gnu-apl--make-key-command-sym (n)
  (intern (concat "gnu-apl-insert-sym-" n)))

(macrolet ((make-insert-functions ()
             `(progn
                ,@(mapcar #'(lambda (command)
                              `(defun ,(gnu-apl--make-key-command-sym (car command)) ()
                                 (interactive)
                                 (insert-string ,(cadr command))))
                          gnu-apl--symbols))))
  (make-insert-functions))

(defun gnu-apl-insert-sym-iota ()
  (interactive)
  (insert-string "⍳"))

(makunbound 'gnu-apl-mode-map)
(defvar gnu-apl-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (command gnu-apl--symbols)
      (let ((key-sequence (caddr command)))
        (when key-sequence
          (define-key map (kbd key-sequence) (gnu-apl--make-key-command-sym (car command))))))
    map))

(define-derived-mode gnu-apl-mode comint-mode "GNU APL"
  "Major mode for interacting with GNU APL."
  (use-local-map gnu-apl-mode-map)
  (setq comint-prompt-regexp "^      "))

(defun gnu-apl ()
  (interactive)
  (let ((buffer (get-buffer-create "*apl*")))
    (pop-to-buffer-same-window buffer)
    (unless (comint-check-proc buffer)
      (make-comint-in-buffer "apl" buffer "/Users/elias/src/apl/dist/bin/apl"
                             nil
                             "--noCIN")
      (gnu-apl-mode))))

(provide 'gnu-apl-mode)
