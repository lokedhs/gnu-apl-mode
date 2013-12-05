;;; -*- lexical-binding: t -*-

(defvar gnu-apl--symbol-doc
  '(("+"
     "Identity" "No change to B"
     "Addition" "Sum of A and B")
    ("−"
     "Negation" "Changes sign of B"
     "Subtraction" "A minus B")
    ("×"
     "Signum" "¨1 if B<0; 0 if B=0; 1 if B>0"
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
    ("↑" nil nil
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
     "Logical NOR" "Logic: 1 if both A and B are 0; otherwise 0")
    ("⍲" nil nil
     "Sheffer stroke" "Logic: 0 if both A and B are 1; otherwise 1")
    ("∼"
     "Not" "Logical: ∼1 is 0, ∼0 is 1" nil)
    ("⍋"
     "Grade up" "Indices of B which will arrange B in ascending order"
     nil nil)
    ("⍒"
     "Grade down" "Indices of B which will arrange B in descending order"
     nil nil)
    ("⍎" "Execute" "Execute an APL expression"
     nil nil)))
