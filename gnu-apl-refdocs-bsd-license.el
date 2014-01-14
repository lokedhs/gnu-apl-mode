;;; -*- lexical-binding: t -*-

(defvar gnu-apl--symbol-doc
  '(("+"
     (("Monadic" "Identity" "No change to B")
      ("Dyadic" "Addition" "Sum of A and B")))
    ("−"
     (("Monadic" "Negation" "Changes sign of B")
      ("Dyadic" "Subtraction" "A minus B")))
    ("×"
     (("Monadic" "Signum" "¯1 if B<0; 0 if B=0; 1 if B>0")
      ("Dyadic" "Multiply" "A multiplied by B")))
    ("÷"
     (("Monadic" "Reciprocal" "1 divided by B")
      ("Dyadic" "Division (mathematics)" "A divided by B")))
    ("⋆"
     (("Monadic" "Exponential" "e to the B power")
      ("Dyadic" "Exponentiation]]" "A raised to the B power")))
    ("○"
     (("Monadic" "Pi times" "Multiply by π")
      ("Dyadic" "Circle" "Trigonometric functions of B selected by A. A=1: sin(B) A=2: cos(B) A=3: tan(B)")))
    ("?"
     (("Monadic" "Roll" "One integer selected randomly from the first B integers")
      ("Dyadic" "Deal" "A distinct integers selected randomly from the first B integers")))
    ("∊"
     (("Monadic" "Enlist" "Create a vector containing all scalars in B")
      ("Dyadic" "Membership" "1 for elements of A present in B; 0 where not.")))
    ("⌈"
     (("Monadic" "Ceiling" "Least integer greater than or equal to B")
      ("Dyadic" "Sample maximum and minimum" "The greater value of A or B")))
    ("⌊"
     (("Monadic" "Floor" "Greatest integer less than or equal to B")
      ("Dyadic" "Sample maximum and minimum" "The smaller value of A or B")))
    ("⍴"
     (("Monadic" "Shape" "Number of components in each dimension of B")
      ("Dyadic" "Reshape" "Array of shape A with data B")))
    ("↑"
     (("Monadic" "Take" "Select the first element of B")
      ("Dyadic" "Take" "Select the first (or last) A elements of B according to ×A")))
    ("↓"
     (("Dyadic" "Drop " "Remove the first (or last) A elements of B according to ×A")))
    ("⊥"
     (("Dyadic" "Decode" "Value of a polynomial whose coefficients are B at A")))
    ("⊤"
     (("Dyadic" "Encode" "Base-A representation of the value of B")))
    ("∣"
     (("Monadic" "Absolute value" "Magnitude of B")
      ("Dyadic" "Modulo" "B modulo A")))
    (","
     (("Monadic" "Ravel" "Reshapes B into a vector")
      ("Dyadic" "Catenation" "Elements of B appended to the elements of A")))
    ("\\"
     (("Dyadic" "Expansion" "Insert zeros (or blanks) in B corresponding to zeros in A")))
    ("/"
     (("Dyadic" "Compress" "Select elements in B corresponding to ones in A")))
    ("⍳"
     (("Monadic" "Index generator" "Vector of the first B integers")
      ("Dyadic" "Index of" "The location (index) of B in A; 1+⌈/⍳⍴A if not found")))
    ("⌹"
     (("Monadic" "Matrix inverse" "Inverse of matrix B")
      ("Dyadic" "Matrix divide" "Solution to system of linear equations Ax = B")))
    ("⌽"
     (("Monadic" "Reversal" "Reverse elements of B along last axis")
      ("Dyadic" "Rotation" "The elements of B are rotated A positions")))
    ("⊖"
     (("Monadic" "Reversal" "Reverse elements of B along first axis")
      ("Dyadic" "Rotation" "The elements of B are rotated A positions along the first axis")))
    ("⍟"
     (("Monadic" "Logarithm" "Natural logarithm of B")
      ("Dyadic" "Logarithm" "Logarithm of B to base A")))
    ("⍕"
     (("Monadic" "Format" "A character representation of B")
      ("Dyadic" "Format" "Format B into a character matrix according to A")))
    ("⍉"
     (("Monadic" "Transpose" "Reverse the axes of B")
      ("Dyadic" "Transpose" "The axes of B are ordered by A")))
    ("!"
     (("Monadic" "Factorial" "Product of integers 1 to B")
      ("Dyadic" "Combinations" "Number of combinations of B taken A at a time")))
    ("<"
     (("Dyadic" "Less than" "Comparison: 1 if true, 0 if false")))
    ("≤"
     (("Dyadic" "Less than or equal" "Comparison: 1 if true, 0 if false")))
    ("="
     (("Dyadic" "Equality" "Comparison: 1 if true, 0 if false")))
    ("≥"
     (("Dyadic" "Greater than or equal" "Comparison: 1 if true, 0 if false")))
    (">"
     (("Dyadic" "Greater than" "Comparison: 1 if true, 0 if false")))
    ("≠"
     (("Dyadic" "Not equal" "Comparison: 1 if true, 0 if false")))
    ("∨"
     (("Dyadic" "Logical disjunction" "Logic: 0 if A and B are 0; 1 otherwise")))
    ("∧"
     (("Dyadic" "Logical conjunction" "Logic: 1 if A and B are 1; 0 otherwise")))
    ("⍱"
     (("Dyadic" "Logical Nor" "Logic: 1 if both A and B are 0; otherwise 0")))
    ("⍲"
     (("Dyadic" "Logical Nand" "Logic: 0 if both A and B are 1; otherwise 1")))
    ("∼"
     (("Dyadic" "Not" "Logical: ∼1 is 0, ∼0 is 1")) nil)
    ("⍋"
     (("Monadic" "Grade up" "Indices of B which will arrange B in ascending order")))
    ("⍒"
     (("Monadic" "Grade down" "Indices of B which will arrange B in descending order")))
    ("⍎"
     (("Monadic" "Execute" "Execute an APL expression")))
    ("←"
     (("Dyadic" "Assignment" "Assign the value of B to A")))
    ("→"
     (("Monadic" "Goto" "Go to line B")))
    ("∇"
     (("Monadic" "Function definition" "Define or modify a function")))
    ("⊂"
     (("Monadic" "Enclose" "Produce a scalar from B")
      ("Dyadic" "Partition" "Divide B into vectors based on A")))
    ("⊃"
     (("Monadic" "Disclose" "Produce an array from B")
      ("Dyadic" "Pick" "Select a value from B based on A")))
    ("∪"
     (("Monadic" "Unique" "Return an array of all unique elements in B")))
    ("⍷"
     (("Dyadic" "Find" "Return a boolean array indicating the positions of the array A in B")))
    ("≡"
     (("Monadic" "Depth" "Return the levels of nesting in B")
      ("Dyadic" "Match" "Returns true if A has the same structure as well as data as B")))
    ("⊥"
     (("Dyadic" "Decode" "Yields the values of array A evaluated in a number system with radices B")))
    ("⊤"
     (("Dyadic" "Encode" "Yields the representation of A in the number system whose radices are B"))))
  "Documentation for APL symbols. Each element is a list of six
elements: The APL symbol, name of monadic operator, description
of the monadic operator, name of the dyadic operator, description
of the dyadic operator, extra documentation.")
