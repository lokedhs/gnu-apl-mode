;;; gnu-apl-osx-workaround.el.el --- GNU APL support for OSX -*- lexical-binding: t -*-

;; Copyright (C) 2013-2015 Elias Mårtenson

;;; Code:

(require 'cl-lib)
(require 'gnu-apl-util)

(defvar gnu-apl--symbols)                   ;gnu-apl-symbols.el

(defun gnu-apl-update-fontset-character (spec)
  (dolist (s gnu-apl--symbols)
    (let ((char (aref (cl-second s) 0)))
      (when (> char 255)
        (set-fontset-font t (cons char char) (font-spec :family spec)))))
  (set-fontset-font t '(#x2500 . #x2594) (font-spec :family spec)))

(provide 'gnu-apl-osx-workaround)
;;; gnu-apl-osx-workaround.el ends here
