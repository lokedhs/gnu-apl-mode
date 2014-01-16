;;; -*- lexical-binding: t -*-

;;;
;;; Each element has the following structure:
;;;
;;;  (SYMBOL
;;;      ((TYPE NAME SUMMARY &optional DESCRIPTION)
;;;       ...)
;;;       IBM-COPYRIGHT-P)
;;;
;;;  SYMBOL - A one-character string or list of alternatives, the actual symbol
;;;  TYPE - The use-type, i.e. "Monadic", "Dyadic"...
;;;  SUMMARY - Short one-line summary
;;;  DESCRIPTION - Long description of the symbol
;;;  IBM-COPYRIGHT-P - T if the text is an APL2 reprint
;;;

(defvar gnu-apl--symbol-doc
  '(("+"
     (("Monadic" "Conjugate" "Z is R with its imaginary part negated"
       "Z←+R

R and Z: Numeric")
      ("Dyadic" "Add" "Adds R to L"
       "Z←L+R

L, R and Z: Numeric"))
     t)

    ;; ========================================

    ("−"
     (("Monadic" "Negation" "Reverses the sign of R"
       "Z←−R
Reverses the sign of R.

R and Z: Numeric
")
      ("Dyadic" "Subtract" "Subtracts R from L"
       "Z←L−R
Subtracts R from L.

L, R, and Z: Numeric"))
     t)

    ;; ========================================

    ("×"
     (("Monadic" "Signum" "¯1 if R<0; 0 if R=0; 1 if R>0"
       "Z←×R
Yields the number of magnitude 1 with the same phase as R for
nonzero R. If R is 0, Z is 0.

R and Z: Numeric")
      ("Dyadic" "Multiply" "Multiplies L by R"
       "Z←L×R
Multiplies L by R.

L, R, and Z: Numeric"))
     t)

    ;; ========================================

    ("÷"
     (("Monadic" "Reciprocal" "1 divided by B")
      ("Dyadic" "Division (mathematics)" "A divided by B")))

    ;; ========================================

    ("⋆"
     (("Monadic" "Exponential" "e to the R power"
       "Z←⋆R
Determines the Rth power of the base of the natural logarithms e,
where e is approximately 2.7182818284590452.
R and Z: Numeric")
      ("Dyadic" "Power" "L raised to the R power"
       "Z←L⋆R
Raises base L to the Rth power.

Z, R and Z: Numeric"))
     t)

    ;; ========================================

    ("○"
     (("Monadic" "Pi times" "Multiply by π")
      ("Dyadic" "Circle" "Trigonometric functions of B selected by A. A=1: sin(B) A=2: cos(B) A=3: tan(B)")))

    ;; ========================================

    ("?"
     (("Monadic" "Roll" "One integer selected randomly from the first B integers")
      ("Dyadic" "Deal" "A distinct integers selected randomly from the first B integers")))

    ;; ========================================

    ("∊"
     (("Monadic" "Enlist" "Create a vector containing all scalars in B")
      ("Dyadic" "Membership" "1 for elements of A present in B; 0 where not.")))

    ;; ========================================

    ("⌈"
     (("Monadic" "Ceiling" "Least integer greater than or equal to B")
      ("Dyadic" "Sample maximum and minimum" "The greater value of A or B")))

    ;; ========================================

    ("⌊"
     (("Monadic" "Floor" "Greatest integer less than or equal to B")
      ("Dyadic" "Sample maximum and minimum" "The smaller value of A or B")))

    ;; ========================================

    ("⍴"
     (("Monadic" "Shape" "Number of components in each dimension of B")
      ("Dyadic" "Reshape" "Array of shape A with data B")))

    ;; ========================================

    ("↑"
     (("Monadic" "Take" "Select the first element of B")
      ("Dyadic" "Take" "Select the first (or last) A elements of B according to ×A")))

    ;; ========================================

    ("↓"
     (("Dyadic" "Drop " "Remove the first (or last) A elements of B according to ×A")))

    ;; ========================================

    (("∣" "|")
     (("Monadic" "Magnitude" "Yields the distance between 0 and R"
       "Z←|R

R: Numeric
Z: Numeric, real")
      ("Dyadic" "Modulo" "The remainder from dividing R by L"
       "Z←L|R
For real positive L and R, the remainder from dividing R by L.
For all numbers, Z is R-L×⌊R÷L+L=0.

Note: ⌊ is computed with a comparison tolerance of zero.

L, R, and Z: Numeric
Implicit Argument: ⎕CT")))

    ;; ========================================

    (","
     (("Monadic" "Ravel" "Creates a vector from the items in R, taken in row-major order"
       "Z←,R

Z: Vector

 ⍴Z ←→ , /⍴R
⍴⍴Z ←→ ,1")
      ("Dyadic" "Catenate" "Elements of R appended to the elements of L"
       "Z←L,R
Joins L and R. If L and R are nonscalar arrays, L and R are
joined along the last axis. If L and R are scalars, Z is a
two-item vector.

¯1↑⍴Z ←→ Case dependent
  ⍴⍴Z ←→ ,/(⍴⍴L),(⍴⍴R),1")
      ("Monadic with axis" "Ravel with axis" "Create an array of R reshaped according to X"
       "Z←,[X]R
Creates an array that contains the items of R reshaped according
to axes X: If X is a fraction, a new axis of length 1 is formed;
if X is an integer, the X axes of R are combined.

X: Simple scalar fraction or simple scalar or vector of
   nonnegative integers or empty

Implicit argument: ⎕IO

 ⍴Z ←→ Depends on the value of X
⍴⍴Z ←→ Depends on the value of X")
      ("Dyadic with axis" "Catenate with axis" "Join L and R along the axis indicated by X"
       "Z←L,[X]R
Joins L and R along the axis indicated by X.

Z: Nonscalar
X: Simple scalar or one item vector, integer: X∊⍳(⍴⍴L) ⍴⍴R

Implicit argument: ⎕IO

 ⍴Z ←→ Case dependent
⍴⍴Z ←→ (⍴⍴L) ⍴⍴R")
      ("Dyadic with axis (fraction)" "Laminate" "Join L and R by forming a new axis of length 2"
       "Z←L,[X]R
Joins L and R by forming a new axis of length 2, which is
filled with L and R.

Z: Nonscalar
X: Simple scalar fraction between ¯1+⎕IO and ⎕IO+(⍴⍴L) ⍴⍴R

Implicit argument: ⎕IO
 ⍴Z ←→ Case dependent
⍴⍴Z ←→ 1+(⍴⍴L)⌈⍴⍴R"))
     )

    ;; ========================================

    ("\\"
     (("Dyadic" "Expand" "Expands the last axis of R under the control of the Boolean vector LO"
       "Z←LO\\R
Positions in Z that correspond to ones in LO are filled with
items of R. Positions in Z that correspond to 0's in LO are
filled with the fill item (↑0ρ ↑R).

LO: Simple Boolean scalar or vector
Z: Nonscalar array

¯1↓⍴Z ←→ 1↓⍴R
¯1↑⍴Z ←→ ⍴,LO
  ⍴⍴Z ←→ ⍴⍴R")
      ("Dyadic with axis" "Expand with axis" "Expands the Xth axis of R under the control of the Boolean
vector LO"
       "Z←LO\\[X]R

Expand with axis is similar to expand, except that expansion
occurs along the Xth axis.

LO: Simple Boolean scalar or vector
R and Z: Nonscalar array
X: Simple scalar or one-item vector, integer: X∊⍳⍴⍴R

Implicit Argument: ⎕IO

 (⍴Z)[,X] ←→ ⍴,LO
 ⍴⍴Z      ←→ ⍴⍴R")
      ("Axis operator" "Scan" "The Ith item along the last axis is determined by the LO-reduction of I↑[⍴⍴R]R"
       "Z←LO\\R

The Ith item along the last axis is determined by the
LO-reduction of I↑[⍴⍴R]R.

LO: Dyadic function

⍴Z ←→ ⍴R
⍴⍴Z ←→ ⍴⍴R"))
     t)

    ;; ========================================

    ("⍀"
     (("Axis with index" "Expand with axis" "Expands the Xth axis of R under the control of the Boolean
vector LO"
       "Z←LO [X]R

LO: Simple Boolean scalar or vector
R and Z: Nonscalar array
X: Simple scalar or one-item vector, integer: X∊⍳⍴⍴R

Implicit Argument: ⎕IO

 (⍴Z)[,X] ←→ ⍴,LO
 ⍴⍴Z      ←→ ⍴⍴R"))
     t)

    ;; ========================================

    ("/"
     (("Dyadic" "Replicate" "Repeats each subarray along the last axis under the control of the vector LO"
       "Z←LO/R
Repeats each subarray along the last axis under the control of
the vector LO.

LO: Simple scalar or vector, integer
 Z: Nonscalar array

¯1↓⍴Z ←→ ¯1↓⍴R
  ⍴⍴Z ←→ ⍴⍴R")
      ("Axis operator" "Reduce" "Evaluate R as if LO is placed between each element"
       "Z←LO/R
Has the effect of placing the function LO between adjacent pairs of items along
the last axis of R and evaluating the resulting expression for each subarray.

LO: Dyadic function

 ⍴Z ←→ 1↓⍴R
⍴⍴Z ←→ 0⌈¯1+⍴⍴R"))
     t)

    ;; ========================================

    ("⍳"
     (("Monadic" "Index generator" "Vector of the first B integers")
      ("Dyadic" "Index of" "The location (index) of B in A; 1+⌈/⍳⍴A if not found")))

    ;; ========================================

    ("⌹"
     (("Monadic" "Matrix inverse" "Inverse of matrix B")
      ("Dyadic" "Matrix divide" "Solution to system of linear equations Ax = B")))

    ;; ========================================

    ("⌽"
     (("Monadic" "Reversal" "Reverse elements of B along last axis")
      ("Dyadic" "Rotation" "The elements of B are rotated A positions")))

    ;; ========================================

    ("⊖"
     (("Monadic" "Reversal" "Reverse elements of B along first axis")
      ("Dyadic" "Rotation" "The elements of B are rotated A positions along the first axis")))

    ;; ========================================

    ("⍟"
     (("Monadic" "Logarithm" "Natural logarithm of B")
      ("Dyadic" "Logarithm" "Logarithm of B to base A")))

    ;; ========================================

    ("⍕"
     (("Monadic" "Format" "A character representation of B")
      ("Dyadic" "Format" "Format B into a character matrix according to A")))

    ;; ========================================

    ("⍉"
     (("Monadic" "Transpose" "Reverse the axes of B")
      ("Dyadic" "Transpose" "The axes of B are ordered by A")))

    ;; ========================================

    ("!"
     (("Monadic" "Factorial" "Product of integers 1 to B")
      ("Dyadic" "Combinations" "Number of combinations of B taken A at a time")))

    ;; ========================================

    ("<"
     (("Dyadic" "Less than" "Comparison: 1 if true, 0 if false")))

    ;; ========================================

    ("≤" nil nil
     "Less than or equal" "Comparison: 1 if true, 0 if false")

    ;; ========================================

    ("="
     (("Dyadic" "Equality" "Comparison: 1 if true, 0 if false")))

    ;; ========================================

    ("≥"
     (("Dyadic" "Greater than or equal" "Comparison: 1 if true, 0 if false")))

    ;; ========================================

    (">"
     (("Dyadic" "Greater than" "Comparison: 1 if true, 0 if false")))

    ;; ========================================

    ("≠"
     (("Dyadic" "Not equal" "Comparison: 1 if true, 0 if false")))

    ;; ========================================

    ("∨"
     (("Dyadic" "Logical disjunction" "Logic: 0 if A and B are 0; 1 otherwise")))

    ;; ========================================

    ("∧"
     (("Dyadic" "Logical conjunction" "Logic: 1 if A and B are 1; 0 otherwise")))

    ;; ========================================

    ("⍱"
     (("Dyadic" "Logical Nor" "Logic: 1 if both A and B are 0; otherwise 0")))

    ;; ========================================

    ("⍲"
     (("Dyadic" "Logical Nand" "Logic: 0 if both A and B are 1; otherwise 1")))

    ;; ========================================

    ("∼"
     (("Dyadic" "Not" "Logical: ∼1 is 0, ∼0 is 1")) nil)

    ;; ========================================

    ("⍋"
     (("Monadic" "Grade up" "Indices of B which will arrange B in ascending order")))

    ;; ========================================

    ("⍒"
     (("Monadic" "Grade down" "Indices of B which will arrange B in descending order")))

    ;; ========================================

    ("⍎"
     (("Monadic" "Execute" "Execute an APL expression")))

    ;; ========================================

    ("←"
     (("Dyadic" "Assignment" "Assign the value of B to A")))

    ;; ========================================

    ("→"
     (("Monadic" "Goto" "Go to line B")))

    ;; ========================================

    ("∇"
     (("Monadic" "Function definition" "Define or modify a function")))

    ;; ========================================

    ("⊂"
     (("Monadic" "Enclose" "Produce a scalar from B")
      ("Dyadic" "Partition" "Divide B into vectors based on A")))

    ;; ========================================

    ("⊃"
     (("Monadic" "Disclose" "Produce an array from B")
      ("Dyadic" "Pick" "Select a value from B based on A")))

    ;; ========================================

    ("∪"
     (("Monadic" "Unique" "Return an array of all unique elements in B")))

    ;; ========================================

    ("⍷"
     (("Dyadic" "Find" "Return a boolean array indicating the positions of the array A in B")))

    ;; ========================================

    ("≡"
     (("Monadic" "Depth" "Return the levels of nesting in B")
      ("Dyadic" "Match" "Returns true if A has the same structure as well as data as B")))

    ;; ========================================

    ("⊥"
     (("Dyadic" "Decode" "Yields the values of array R evaluated in a number system with radices L"
       "Z←L⊥R
Yields the values of array R evaluated in a number system with
radices L.

L, R, and Z: Simple numeric array

⍴Z ←→ (1↓L),1↓⍴R
⍴⍴Z ←→ (0 1+⍴⍴L)+(0⌈¯1+⍴⍴R)")) t)

    ;; ========================================

    ("⊤"
     (("Dyadic" "Encode" "Yields the representation of R in the number system whose radices are L"
       "Z←L⊤R
Yields the representation of R in the number system whose radices
are L.

L, R, and Z: Simple numeric array

⍴Z ←→ (⍴L),⍴R
⍴⍴Z ←→ (⍴⍴L)+⍴⍴R")) t))
  "Documentation for APL symbols.")
