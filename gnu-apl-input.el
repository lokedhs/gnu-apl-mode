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

(defun gnu-apl--parse-kbd (name)
  (cond ((= (length name) 1)
         name)
        ((and (= (length name) 3) (string= (subseq name 0 2) "S-"))
         (upcase (string (aref name 2))))
        (t
         (error "Unexpected key definition"))))

(macrolet ((make-quail-define-rules ()
             `(quail-define-rules
               ,@(loop for command in gnu-apl--symbols
                       when (third command)
                       collect (list (second command)
                                     (concat "." (gnu-apl--parse-kbd (third command))))))))
  (make-quail-define-rules))
