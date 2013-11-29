;;; -*- lexical-binding: t -*-

(require 'quail)

(quail-define-package "APL-Z" "UTF-8" "⍞" t
                      "Input mode for APL"
                      '(("\t" . quail-completion))
                      t                 ; forget-last-selection
                      nil               ; deterministic
                      nil               ; kbd-translate
                      t                 ; show-layout
                      nil               ; create-decode-map
                      nil               ; maximum-shortest
                      nil               ; overlay-plist
                      nil               ; update-translation-function
                      nil               ; conversion-keys
                      t                 ; simple
                      )

(quail-select-package "APL-Z")

(macrolet ((make-quail-define-rules ()
             `(quail-define-rules
               ,@(loop for command in gnu-apl--symbols
                       for key-command = (fourth command)
                       append (loop for s in (if (listp key-command) key-command (list key-command))
                                    collect (list (concat "." s)
                                                  (second command)))))))
  (make-quail-define-rules))
