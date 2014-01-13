;;; -*- lexical-binding: t -*-

(defvar gnu-apl--symbol-doc
  '(("+"
     "Conjugate" "Z is R with its imaginary part negated"
     "Add" "Adds R to L"
     "Monadic: Conjugate
Z←+R
Z is R with its imaginary part negated

R and Z: Numeric

===================================

Dyadic: Add
Z←L+R
Adds R to L

L, R and Z: Numeric" t)
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
     "Exponential" "e to the R power"
     "Power" "L raised to the R power"
     "Monadic: Exponential
Z←⋆R
Determines the Rth power of the base of the natural logarithms e,
where e is approximately 2.7182818284590452.
R and Z: Numeric

Dyadic: Power
Z←L⋆R
Raises base L to the Rth power.
Z, R and Z: Numeric" t)
    ("○"
     "Pi times" "Multiply by π"
     "Circle" "Trigonometric functions of B selected by A. A=1: sin(B) A=2: cos(B) A=3: tan(B)")
    ("?"
     "Roll" "One integer selected randomly from the first B integers"
     "Deal" "A distinct integers selected randomly from the first B integers")
    ("∊"
     "Enlist" "Create a vector containing all scalars in B"
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
    ("↓"
     nil nil
     "Drop " "Remove the first (or last) A elements of B according to ×A")
    ("⊥"
     nil nil
     "Decode" "Value of a polynomial whose coefficients are B at A")
    ("⊤"
     nil nil
     "Encode" "Base-A representation of the value of B")
    ("∣"
     "Absolute value" "Magnitude of B"
     "Modulo" "B modulo A")
    (","
     "Ravel" "Creates a vector from the items in R, taken in row-major order"
     "Catenate" "Elements of R appended to the elements of L"
     "Monadic: Ravel
Z←,R
Creates a vector from the items in R, taken in row-major order.

Z: Vector

 ⍴Z ←→ , /⍴R
⍴⍴Z ←→ ,1

===================================

,[] Ravel with axis
Z←,[X]R
Creates an array that contains the items of R reshaped according
to axes X: If X is a fraction, a new axis of length 1 is formed;
if X is an integer, the X axes of R are combined.

X: Simple scalar fraction or simple scalar or vector of
   nonnegative integers or empty

Implicit argument: ⎕IO

 ⍴Z ←→ Depends on the value of X
⍴⍴Z ←→ Depends on the value of X

===================================

Dyadic: Catenate
Z←L,R
Joins L and R. If L and R are nonscalar arrays, L and R are
joined along the last axis. If L and R are scalars, Z is a
two-item vector.

¯1↑⍴Z  ←→ Case dependent
   ⍴⍴Z ←→ ,/(⍴⍴L),(⍴⍴R),1

===================================

,[] Catenate with axis
Z←L,[X]R
Joins L and R along the axis indicated by X.

Z: Nonscalar
X: Simple scalar or one item vector, integer: X∊⍳(⍴⍴L) ⍴⍴R

Implicit argument: ⎕IO

 ⍴Z ←→ Case dependent; see below.
⍴⍴Z ←→ (⍴⍴L) ⍴⍴R

=========================

,[] Laminate
Z←L,[X]R
Joins L and R by forming a new axis of length 2, which is
filled with L and R.

Z: Nonscalar
X: Simple scalar fraction between ¯1+⎕IO and ⎕IO+(⍴⍴L) ⍴⍴R

Implicit argument: ⎕IO
 ⍴Z ←→ Case dependent
⍴⍴Z ←→ 1+(⍴⍴L)⌈⍴⍴R" t)
    ("\\"
     nil nil
     "Expansion" "Insert zeros (or blanks) in B corresponding to zeros in A")
    ("/"
     nil nil
     "Replicate" "Repeats each subarray along the last axis under the control of the vector LO"
     "Dyadic: Replicate
Z←LO/R
Repeats each subarray along the last axis under the control of
the vector LO.

LO: Simple scalar or vector, integer
 Z: Nonscalar array

¯1↓⍴Z ←→ ¯1↓⍴R
  ⍴⍴Z ←→ ⍴⍴R

===================================

Axis operator: Reduce
Z←LO/R
Has the effect of placing the function LO between adjacent pairs of items along
the last axis of R and evaluating the resulting expression for each subarray.

LO: Dyadic function

 ⍴Z ←→ 1↓⍴R
⍴⍴Z ←→ 0⌈¯1+⍴⍴R" t)
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
     nil nil)
    ("⍷"
     nil nil
     "Find" "Return a boolean array indicating the positions of the array A in B")
    ("≡"
     "Depth" "Return the levels of nesting in B"
     "Match" "Returns true if A has the same structure as well as data as B")
    ("⊥"
     nil nil
     "Decode" "Yields the values of array R evaluated in a number system with radices L"
     "Z←L⊥R Yields the values of array R evaluated in a number system with
radices L.

L, R, and Z: Simple numeric array

⍴Z ←→ (1↓L),1↓⍴R
⍴⍴Z ←→ (0 1+⍴⍴L)+(0⌈¯1+⍴⍴R)" t)
    ("⊤"
     nil nil
     "Encode" "Yields the representation of R in the number system whose radices are L"
     "Z←L⊤R
Yields the representation of R in the number system whose radices
are L.

L, R, and Z: Simple numeric array

⍴Z ←→ (⍴L),⍴R
⍴⍴Z ←→ (⍴⍴L)+⍴⍴R" t))
  "Documentation for APL symbols. Each element is a list of six
elements: The APL symbol, name of monadic operator, description
of the monadic operator, name of the dyadic operator, description
of the dyadic operator, extra documentation.")
