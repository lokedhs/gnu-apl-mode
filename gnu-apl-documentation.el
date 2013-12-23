;;; -*- lexical-binding: t -*-

(defvar gnu-apl--symbol-doc
  '(("+"
     "Identity" "No change to B"
     "Addition" "Sum of A and B")
    ("−"
     "Negation" "Changes sign of B"
     "Subtraction" "A minus B")
    ("×"
     "Signum" "¯1 if B<0; 0 if B=0; 1 if B>0"
     "Multiply" "A multiplied by B")
    ("÷"
     "Reciprocal" "1 divided by B"
     "Division (mathematics)" "A divided by B")
    ("⋆"
     "Exponential" "e to the B power"
     "Exponentiation]]" "A raised to the B power")
    ("○"
     "Pi times" "Multiply by π"
     "Circle" "Trigonometric functions of B selected by A. A=1: sin(B) A=2: cos(B) A=3: tan(B)")
    ("?"
     "Roll" "One integer selected randomly from the first B integers"
     "Deal" "A distinct integers selected randomly from the first B integers")
    ("∈" nil nil
     "Membership" "1 for elements of A present in B; 0 where not.")
    ("⌈"
     "Ceiling" "Least integer greater than or equal to B"
     "Sample maximum and minimum" "The greater value of A or B")
    ("⌊"
     "Floor" "Greatest integer less than or equal to B"
     "Sample maximum and minimum" "The smaller value of A or B")
    ("⍴"
     "Shape" "Number of components in each dimension of B"
     "Reshape" "Array of shape A with data B")
    ("↑"
     "Take" "Select the first element of B"
     "Take" "Select the first (or last) A elements of B according to ×A")
    ("↓" nil nil
     "Drop " "Remove the first (or last) A elements of B according to ×A")
    ("⊥" nil nil
     "Decode" "Value of a polynomial whose coefficients are B at A")
    ("⊤" nil nil
     "Encode" "Base-A representation of the value of B")
    ("∣"
     "Absolute value" "Magnitude of B"
     "Modulo" "B modulo A")
    (","
     "Ravel" "Reshapes B into a vector"
     "Catenation" "Elements of B appended to the elements of A")
    ("\\" nil nil
     "Expansion" "Insert zeros (or blanks) in B corresponding to zeros in A")
    ("/" nil nil
     "Compression" "Select elements in B corresponding to ones in A")
    ("⍳"
     "Index generator" "Vector of the first B integers"
     "Index of" "The location (index) of B in A; 1+⌈/⍳⍴A if not found")
    ("⌹"
     "Matrix inverse" "Inverse of matrix B"
     "Matrix divide" "Solution to system of linear equations Ax = B")
    ("⌽"
     "Reversal" "Reverse elements of B along last axis"
     "Rotation" "The elements of B are rotated A positions")
    ("⊖"
     "Reversal" "Reverse elements of B along first axis"
     "Rotation" "The elements of B are rotated A positions along the first axis")
    ("⍟"
     "Logarithm" "Natural logarithm of B"
     "Logarithm" "Logarithm of B to base A")
    ("⍕"
     "Format" "A character representation of B"
     "Format" "Format B into a character matrix according to A")
    ("⍉"
     "Transpose" "Reverse the axes of B"
     "Transpose" "The axes of B are ordered by A")
    ("!"
     "Factorial" "Product of integers 1 to B"
     "Combinations" "Number of combinations of B taken A at a time")
    ("<" nil nil
     "Less than" "Comparison: 1 if true, 0 if false")
    ("≤" nil nil
     "Less than or equal" "Comparison: 1 if true, 0 if false")
    ("=" nil nil
     "Equality" "Comparison: 1 if true, 0 if false")
    ("≥" nil nil
     "Greater than or equal" "Comparison: 1 if true, 0 if false")
    (">" nil nil
     "Greater than" "Comparison: 1 if true, 0 if false")
    ("≠" nil nil
     "Not equal" "Comparison: 1 if true, 0 if false")
    ("∨" nil nil
     "Logical disjunction" "Logic: 0 if A and B are 0; 1 otherwise")
    ("∧" nil nil
     "Logical conjunction" "Logic: 1 if A and B are 1; 0 otherwise")
    ("⍱" nil nil
     "Logical Nor" "Logic: 1 if both A and B are 0; otherwise 0")
    ("⍲" nil nil
     "Logical Nand" "Logic: 0 if both A and B are 1; otherwise 1")
    ("∼" nil nil
     "Not" "Logical: ∼1 is 0, ∼0 is 1" nil)
    ("⍋"
     "Grade up" "Indices of B which will arrange B in ascending order"
     nil nil)
    ("⍒"
     "Grade down" "Indices of B which will arrange B in descending order"
     nil nil)
    ("⍎"
     "Execute" "Execute an APL expression"
     nil nil)
    ("←" nil nil
     "Assignment" "Assign the value of B to A")
    ("→"
     "Goto" "Go to line B"
     nil nil)
    ("∇"
     "Function definition" "Define or modify a function"
     nil nil)
    ("⊂"
     "Enclose" "Produce a scalar from B"
     "Partition" "Divide B into vectors based on A")
    ("⊃"
     "Disclose" "Produce an array from B"
     "Pick" "Select a value from B based on A")
    ("∪"
     "Unique" "Return an array of all unique elements in B"
     nil nil)))

;;;
;;; Keymap buffer
;;;

(defvar *gnu-apl-keymap-buffer-name* "*gnu-apl keymap*")

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
    (let ((buffer (get-buffer-create *gnu-apl-keymap-buffer-name*)))
      (with-current-buffer buffer
        (delete-region (point-min) (point-max))
        (insert keymap-template)
        (goto-char (point-min))
        (while (search-forward-regexp "\\(.\\)∇" nil t)
          (let* ((key (match-string 1))
                 (found (cl-find key gnu-apl--symbols :key #'third :test #'equal))
                 (result-string (if found (second found) " ")))
            (replace-match (concat key result-string) t t)))
        (gnu-apl-keymap-mode))
      buffer)))

(defun gnu-apl-show-keyboard ()
  (interactive)
  (let ((buffer (or (when nil ; Make sure the buffer is always created
                      (get-buffer *gnu-apl-keymap-buffer-name*))
                    (gnu-apl--make-readable-keymap))))
    (let ((window (split-window nil (- (with-current-buffer buffer (1+ (count-lines (point-min) (point-max))))))))
      (set-window-buffer window buffer))))

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
  (labels ((make-doc-message (name short long)
             (when (and short long)
               (format "%s: %s: %s" name short long))))
    (when (looking-at (concat "\\(" gnu-apl--function-regexp "\\)"))
      (let* ((symbol (match-string 1))
             (doc (cl-find symbol gnu-apl--symbol-doc :test #'equal :key #'car)))
        (unless doc
          (error "doc should not be null"))
        ;; We have a documentation entry. Now we need to figure out if the call
        ;; is monadic or dyadic. It can be done by searching backwards until we hit
        ;; a non-space character or the beginning of the line.
        (if (gnu-apl--is-point-on-argument-value)
            (make-doc-message "Dyadic" (fourth doc) (fifth doc))
          (make-doc-message "Monadic" (second doc) (third doc)))))))
