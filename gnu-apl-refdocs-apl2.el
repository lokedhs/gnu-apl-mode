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

    (("-" "−")
     (("Monadic" "Negation" "Reverses the sign of R"
       "Z←−R

R and Z: Numeric
")
      ("Dyadic" "Subtract" "Subtracts R from L"
       "Z←L−R

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

L, R, and Z: Numeric"))
     t)

    ;; ========================================

    ("÷"
     (("Monadic" "Reciprocal" "1 divided by R"
       "Z←÷R
Divides 1 by R.

R and Z: Numeric, nonzero")
      ("Dyadic" "Divide" "Divides L by R"
       "Z←L÷R
Divides L by R.

L, R, and Z: Numeric"))
     t)

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
     (("Monadic" "Pi times" "Multiply by π"
       "Z←○R
Multiplies any number by π (approximately
3.1415926535897933).

R and Z: Numeric")
      ("Dyadic" "Circle functions" "Trigonometric functions of R selected by L"
       "Z←L○R
L determines which of a family of circular, hyperbolic, Pythagorean,
and complex number functions to apply to R.

L: Integer such that ¯12≤L and L≤12
R and Z: Numeric

Functions:

 L     L○R            L      L○R
  0   (1-R⋆2)⋆5
 ¯1   arcsin R          1   sin R
 ¯2   arccos R          2   cosin R
 ¯3   arctan R          3   tan R
 ¯4   (1+R⋆2)⋆5         4   (1+R⋆2)⋆5
 ¯5   arcsinh R         5   sinh R 
 ¯6   arccosh R         6   cosh R
 ¯7   arctanh R         7   tanh R
 ¯8   -(8○R)            8   -(1-R⋆2)⋆0.5 for R≥0, (¯1-R⋆2)⋆0.5 for R<0
 ¯9   R                 9   Real R
¯10   +R               10   |R
¯11   0J1 R            11   Imaginary R
¯12   ⋆0J1 R           12   Phase R"))
     t)

    ;; ========================================

    ("?"
     (("Monadic" "Roll" "Selects an integer at random from the population ⍳R"
"Z←?R

R: Positive integer
Z: Integer in the set ιR

Implicit arguments: ⎕IO and ⎕RL")
      ("Dyadic" "Deal" "Selects L integers at random from the population ⍳R without replacement"
       "Z←L?R

L and R: Simple scalar or one-item vector, nonnegative integer
Z: Simple vector, integer in set ⍳R

Implicit arguments: ⎕IO and ⎕RL

 ⍴Z ←→ ,L
⍴⍴Z ←→ ,1"))
     t)

    ;; ========================================

    (("∊" "ε")
     (("Monadic" "Enlist" "Creates a simple vector whose items are the simple scalars in R"
       "Z←∊R

Z: Simple vector

 ⍴Z ←→ Number of simple scalars in R
⍴⍴Z ←→ ,1")
      ("Dyadic" "Member" "1 for elements of L present in R; 0 where not."
"Z←L∊R

Yields a Boolean array Z with the same shape as L. An item of Z is
1 if the corresponding item of L can be found anywhere in R. An
item of Z is 0 otherwise.

Z: Simple Boolean array

Implicit argument: ⎕CT

 ⍴Z ←→ ⍴L
⍴⍴Z ←→ ⍴⍴L"))
     t)

    ;; ========================================

    ("⌈"
     (("Monadic" "Ceiling" "Least integer greater than or equal to R"
"Z←⌈R
For real numbers, yields the smallest integer that is not
less than R (within the comparison tolerance).
For complex numbers, depends on the relationship of the real
and imaginary parts of R.

R and Z: Numeric

Implicit argument: ⎕CT")
      ("Dyadic" "Maximum" "The greater value of L or R"
"Z←L⌈R
Returns the larger of L and R.

L, R, and Z: Numeric, real"))
     t)

    ;; ========================================

    ("⌊"
     (("Monadic" "Floor" "Greatest integer less than or equal to R"
       "Z←⌊R

For real numbers, yields the largest integer that does not exceed
R (within the comparison tolerance).

For complex numbers, depends on the relationship of the real and
imaginary parts of R.

R and Z: numeric

Implicit Argument: ⎕CT")
      ("Dyadic" "Minimum" "The smaller value of L or R"
       "Z←L⌊R
Returns the smaller of L and R.

L, R and Z: Numeric, real"))
     t)

    ;; ========================================

    ("⍴"
     (("Monadic" "Shape" "Yields the size of each axis of R"
       "Z←⍴R
Yields the size of each axis of R.

Z: Simple nonnegative integer vector.

 ⍴Z ←→ ⍴⍴R
⍴⍴Z ←→ ,1")
      ("Dyadic" "Reshape" "Structures the items of R into an array of shape L"
       "Z←L⍴R

L: Simple scalar or vector, not negative integers.

 ⍴Z ←→ ,L
⍴⍴Z ←→ ⍴,L"))
     t)

    ;; ========================================

    ("↑"
     (("Monadic" "First" "Selects the first item of R taken in row major order"
       "Z←↑R
Selects the first item of R taken in row major order. If R is empty,
yields the prototype of R.

 ⍴Z ←→ Depends on shape of the first item
⍴⍴Z ←→ Depends on rank of the first item")
      ("Dyadic" "Take" "Select the first or last L elements of R"
       "Z←L↑R
Selects subarrays from the beginning or end of the
Ith axis of R, according to whether L[I]
is positive or negative.

L: Simple scalar or vector, integer

 ⍴Z ←→ ,L
⍴⍴Z ←→ ⍴,L")
      ("Dyadic with axis" "Take with axis" "Select the first or last L elements of R"
       "Z←L↑[X]R
Selects subarrays from the beginning or end of
the X[I]th axis of R, according to whether
L[I] is positive or negative.

L: Simple scalar or vector, integer
R and Z: Nonscalar array
X: Simple scalar or vector; nonnegative integers: X∊⍳⍴⍴R; or empty

Implicit argument: ⎕IO

 (⍴Z)[,X] ←→ ,L
      ⍴⍴Z ←→ ⍴⍴R"))
     t)

    ;; ========================================

    ("↓"
     (("Dyadic" "Drop" "Remove the first or last L elements of R"
       "Z←L↓R
Removes subarrays from the beginning or end of the Ith axis of R,
according to whether L[I] is positive or negative.

L: Simple scalar or vector, integer

Z: Nonscalar array
 ⍴Z ←→ 0 (⍴R)| L
⍴⍴Z ←→ (⍴,L) ⍴⍴R")
      ("Dyadic with axis" "Drop with axis" "Remove the first or last L elements of R"
       "↓[] Drop with Axis
Z←L↓[X]R
Removes subarrays from the beginning or end of the X[I]th
axis of R, according to whether L[I] is positive or negative.

L: Simple scalar or vector, integer
R and Z: Nonscalar array
X: Simple scalar or vector; nonnegative integers: Xε⍳⍴⍴R; or empty

Implicit argument: ⎕IO

 (⍴Z)[,X] ←→ 0 (⍴R)[,X]| L
      ⍴⍴Z ←→ ⍴⍴R"))
     t)

    ;; ========================================

    (("|" "∣")
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

Implicit Argument: ⎕CT"))
     t)

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
filled with the fill item (↑0⍴⊂↑R).

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
     (("Monadic" "Interval" "Vector of the first R integers"
       "Z←⍳R
Produces R consecutive ascending integers, beginning with ⎕IO.

R: Simple scalar or one-item vector, nonnegative integer
Z: Simple vector, nonnegative integers

Implicit argument: ⎕IO

 ⍴Z ←→ ,R
⍴⍴Z ←→ ,1")
      ("Dyadic" "Index of" "The location (index) of B in A; 1+⌈/⍳⍴A if not found"
       "Z←L⍳R
Yields the first occurrence in L of items in R.

L: Vector
Z: Nonnegative integers

Implicit arguments: ⎕IO, ⎕CT

 ⍴Z ←→ ⍴R
⍴⍴Z ←→ ⍴⍴R"))
     t)

    ;; ========================================

    ("."
     ("Axis operator" "Inner product" "Compute inner product of from LO and RO"
      "Z←L LO.RO R
Combines the subarrays along the last axis of L with subarrays
along the first axis of R by applying an RO outer product. An
LO-reduction is then applied to each item of that result.

LO: Dyadic function
RO: Dyadic function
 ⍴Z ←→ (¯1↓⍴L),1↓⍴R
⍴⍴Z ←→ ,0⌈¯2+(⍴⍴L)+⍴⍴R")
     t)

    ;; ========================================

    ("⌹"
     (("Monadic" "Matrix inverse" "Inverse of matrix B"
       "Z←⌹R
Yields the inverse of a nonsingular matrix. Results for other
matrixes, vectors, and scalar R are discussed below.

R and Z: Simple numeric array of rank 2 or less

 ⍴Z ←→ ⍴R
⍴⍴Z ←→ ⍴⍴R")
      ("Dyadic" "Matrix divide" "Yields the solution to system of linear equations"
       "Z←L⌹R
Yields the solution of a system of linear equations or other algebraic
or geometric results, according to the values and shapes of L and R.

L and R: Simple numeric array of rank 2 or less
Z: Simple numeric

 ⍴Z ←→ (1↓⍴R),1↓⍴L
⍴⍴Z ←→ ,1¯2+(⍴⍴L)+⍴⍴R"))
     t)

    ;; ========================================

    ("⌽"
     (("Monadic" "Reverse" "Creates an array with the items of R reversed along the last axis"
       "Z←⌽R
Creates an array with the items of R
reversed along the last axis.

 ⍴Z ←→ ⍴R
⍴⍴Z ←→ ⍴⍴R")
      ("Monadic with axis" "Reverse with axis" "Creates an array with items reversed along the Xth axis"
       "Z←⌽[X]R

X: Simple scalar or one-item vector, integer: X∊⍳⍴⍴R

Implicit argument: ⎕IO

 ⍴Z ←→ ⍴R
⍴⍴Z ←→ ⍴⍴R")
      ("Dyadic" "Rotate" "The elements of R are rotated L positions"
       "Z←L⌽R
Creates an array with items of R rotated
L positions along the last axis.
The sign of L determines the direction of
the rotation.

L: Simple integer, either scalar or rank ¯1+⍴⍴R

 ⍴Z ←→ ⍴R
⍴⍴Z ←→ ⍴⍴R")
      ("Dyadic with axis" "Rotate with axis" "Creates an array with items of R rotated |L positions along the Xth axis"
       "Z←L⌽[X]R
Creates an array with items of R rotated
|L positions along the Xth axis.
The sign of L determines the direction
of the rotation.

L: Simple integer, scalar, or vector
X: Simple scalar or one-item vector, integer: X∊ι⍴⍴R

Implicit argument: ⎕IO

 ⍴Z ←→ ⍴R
⍴⍴Z ←→ ⍴⍴R"))
     t)

    ;; ========================================

    ("⊖"
     (("Monadic" "Reverse" "Creates an array with the items of R reversed along the first axis."
       "Z←⊖R
Creates an array with the items of R
reversed along the first axis.

 ⍴Z ←→ ⍴R
⍴⍴Z ←→ ⍴⍴R")
      ("Monadic with axis" "Reverse with axis" "Creates an array with items reversed along the Xth axis"
       "Z←⊖[X]R Creates an array with items reversed along the
Xth axis.

X: Simple scalar or one-item vector, integer: X∊ι⍴⍴R

Implicit argument: ⎕IO

 ⍴Z ←→ ⍴R
⍴⍴Z ←→ ⍴⍴R")
      ("Dyadic" "Rotation" "The elements of R are rotated L positions along the first axis"
       "Z←L⊖R
Creates an array with items of R rotated
L positions along the first axis.
The sign of L determines the direction of
the rotation.

 ⍴Z ←→ ⍴R
⍴⍴Z ←→ ⍴⍴R"))
     t)

    ;; ========================================

    ("⍟"
     (("Monadic" "Logarithm" "Natural logarithm of R"
       "Z←⍟R
Determines the logarithm of R to the base of the natural logarithms
e, where e is approximately 2.7182818284590452.

R: Numeric, nonzero
Z: Numeric")
      ("Dyadic" "Logarithm" "Determines the base L logarithm of R"
       "Z←L⍟R
Determines the base L logarithm of R.

L and R: Numeric, nonzero
Z: Numeric"))
     t)

    ;; ========================================

    ("⍕"
     (("Monadic" "Format" "A character representation of R"
       "Z←⍕R
Creates a simple character array whose appearance is the same as
the display of R (if PW is set sufficiently wide.)

Z: Character array

Implicit argument: ⎕PP

 ⍴Z ←→ See APL2 Programming: Language Reference
⍴⍴Z ←→ ,1 ⍴⍴R if R is simple
⍴⍴Z ←→ ,1; or ,2 if R is nested")
      ("Dyadic" "Format by specification" "Format R into a character matrix according to L"
       "Z←L⍕R
Transforms R to a character array that displays according to column
specifications L. Each pair of L corresponds to a column. The first
of the pair sets column width; the second sets display precision and
format – either conventional or scaled.
A single pair of integers extends the specification to all columns. A
single integer is interpreted as (0,L).

L: Simple integer vector
R: Array of depth 2 or less, whose items are simple
real scalars or simple character scalars or vectors
Z: Simple character array

Implicit argument: ⎕FC[1 4 6]

1↓⍴Z ←→ 1↓⍴R
 ⍴⍴Z ←→ 1 ⍴⍴R"))
     t)

    ;; ========================================

    ("⍉"
     (("Monadic" "Transpose" "Creates an array similar to R but with the order of the axes of R reversed"
       "Z←⍉R
Creates an array similar to R but with the order
of the axes of R reversed.

 ⍴Z ←→ ⌽⍴R
⍴⍴Z ←→ ⍴⍴R")
      ("Dyadic" "Transpose" "The axes of R are ordered by R"
       "Z←L⍉R
Case 1: L selects all axes of R. Creates an
array similar to R but with the axes permuted
according to L.

Case 2: L includes repetitions of axes. Creates
an array with two or more axes of R mapped
into a single axis of Z, which is then a diagonal
cross section of R.

L: Simple scalar or vector, nonnegative integer

Implicit Argument: ⎕IO

Case 1
         ⍴Z ←→ (⍴R)[⍋L]
        ⍴⍴Z ←→ ⍴⍴R
Case 2
       I⊃⍴Z ←→ ⌊/(L=I)/⍴R
        (for each I∊⍳⍴⍴Z)

        ⍴⍴Z ←→ ,+/(LιL)=⍳⍴L"))
     t)

    ;; ========================================

    ("!"
     (("Monadic" "Factorial" "Product of integers 1 to R"
       "Z←!R
For positive integer R, yields the product of all positive integers
through R.

For all numbers but negative integers, factorial yields the Gamma
function of R+1.

R: Numeric, except for negative integers
Z: Numeric

Gamma Function: Factorial approximates the gamma function of (n+1).")
      ("Dyadic" "Binomial" "For nonnegative integer arguments, yields the number of distinct combinations of R things taken L at a time"
       "Z←L!R
For nonnegative integer arguments, yields the number of distinct
combinations of R things taken L at a time.

In the following table, <0 means that L, R, or R−L is a negative integer and
≥0 means that L, R, or R−L is a nonnegative integer. The corresponding
definition is used.

Case         Definition
L    R   R|L
≮0  ≮0   ≮0  Return (!R)÷(!L)×!R−L
≮0  ≮0   <0  Return 0
≮0  <0   ≮0  (Case cannot occur.)
≮0  <0   <0  Return (¯1*L)×L!L−R+1
<0  ≮0   ≮0  Return 0
<0  ≮0   <0  (Case cannot occur.)
<0  <0   ≮0  Return (¯1*R−L) (−R+1)!(| L+1)
<0  <0   <0  Return 0"))
     t)

    ;; ========================================

    ("<"
     (("Dyadic" "Less than" "Comparison: 1 if true, 0 if false"
       "Z←L<R
Less than

L and R Numeric real
Z: Boolean
Implicit Argument: ⎕CT"))
     t)

    ;; ========================================

    ("≤"
     (("Dyadic" "Less than or equal" "Comparison: 1 if true, 0 if false"
       "Z←L≤R
Less than or equal

L and R Numeric real
Z: Boolean
Implicit Argument: ⎕CT"))
     t)

    ;; ========================================

    ("="
     (("Dyadic" "Equality" "Comparison: 1 if true, 0 if false"
       "Z←L=R
Equal

Z: Boolean
Implicit Argument: ⎕CT"))
     t)

    ;; ========================================

    ("≥"
     (("Dyadic" "Greater than or equal" "Comparison: 1 if true, 0 if false"
       "Z←L≥R
Greater than or equal

L and R Numeric real
Z: Boolean
Implicit Argument: ⎕CT"))
     t)

    ;; ========================================

    (">"
     (("Dyadic" "Greater than" "Comparison: 1 if true, 0 if false"
       "Z←L>R
Greater than

L and R Numeric real
Z: Boolean
Implicit Argument: ⎕CT"))
     t)

    ;; ========================================

    ("≠"
     (("Dyadic" "Not equal" "Comparison: 1 if true, 0 if false"
       "Z←L≠R
Not equal

Z: Boolean
Implicit Argument: ⎕CT"))
     t)

    ;; ========================================

    ("∨"
     (("Dyadic" "Or" "Logic: 0 if A and B are 0; 1 otherwise"
       "Z←L∨R
Or

L, R, and Z: Boolean"))
     t)

    ;; ========================================

    (("∧" "^")
     (("Dyadic" "And" "Logic: 1 if A and B are 1; 0 otherwise"
       "Z←L∧R
And

L, R, and Z: Boolean"))
     t)

    ;; ========================================

    ("⍱"
     (("Dyadic" "Nor" "Logic: 1 if both A and B are 0; otherwise 0"
       "Z←L⍱R
Nor

L, R, and Z: Boolean"))
     t)

    ;; ========================================

    ("⍲"
     (("Dyadic" "Nand" "Logic: 0 if both A and B are 1; otherwise 1"
       "Z←L⍲R
Nand

L, R, and Z: Boolean"))
     t)

    ;; ========================================

    ("⍋"
     (("Monadic" "Grade up" "Indices of R which will arrange R in ascending order"
       "Z←⍋R
Yields a vector of integers (a permutation of ⍳1↑⍴R) that puts the
subarrays along the first axis of R in ascending order.

R: Simple nonscalar numeric array
Z: Simple vector nonnegative integers

Implicit argument: ⎕IO

 ⍴Z ←→ 1↑⍴R
⍴⍴Z ←→ ,1")
      ("Dyadic" "Grade up" "Indices of R which will arrange R in ascending order based on collating sequence specified as L"
       "Z←L⍋R
Yields a vector of integers (a permutation of ⍳1↑⍴R) that puts the
subarrays along the first axis of R in ascending order according to
the collating sequence L.

L: Simple nonempty nonscalar character array
R: Simple nonscalar character array
Z: Simple vector, nonnegative integers

Implicit argument: ⎕IO

 ⍴Z ←→ 1↑⍴R
⍴⍴Z ←→ ,1"))
     t)

    ;; ========================================

    ("⍒"
     (("Monadic" "Grade down" "Indices of R which will arrange R in descending order"
       "Z←⍒R
Yields a vector of integers (a permutation of ⍳1↑⍴R) that puts the
subarrays along the first axis of R in decending order.

R: Simple nonscalar numeric array
Z: Simple vector nonnegative integers

Implicit argument: ⎕IO

 ⍴Z ←→ 1↑⍴R
⍴⍴Z ←→ ,1")
      ("Dyadic" "Grade down" "Indices of R which will arrange R in descending order based on collating sequence specified as L"
       "Z←L⍒R
Yields a vector of integers (a permutation of ⍳1↑⍴R) that puts the
subarrays along the first axis of R in descending order according to
the collating sequence L.

L: Simple nonempty nonscalar character array
R: Simple nonscalar character array
Z: Simple vector, nonnegative integers

Implicit argument: ⎕IO

 ⍴Z ←→ 1↑⍴R
⍴⍴Z ←→ ,1"))
     t)

    ;; ========================================

    ("⍎"
     (("Monadic" "Execute" "Evaluates the statement represented by the character vector R"
       "Z←⍎R
Evaluates the statement represented by the character vector R.

R: Simple character scalar or vector

 ⍴Z ←→ Data dependent
⍴⍴Z ←→ Data dependent"))
     t)

    ;; ========================================

    ("←"
     (("Dyadic" "Assignment" "Specification or assignment arrow"
       "←
Associates a name with an array, or modifies the values of selected posi-
tions in an array already associated with a name."))
     t)

    ;; ========================================

    ("→"
     (("Monadic" "Goto" "Branch or escape arrow"
       "→
Followed by an expression, indicates the next line, if any, in a
defined function or operator to be executed. Alone, clears the
state indicator of a suspended operation and its entire calling
sequence."))
     t)

    ;; ========================================

    ("∇"
     (("Monadic" "Function definition" "Define or modify a function")))

    ;; ========================================

    ("⊂"
     (("Monadic" "Enclose" "Creates a scalar array whose only item is R"
       "Z←⊂R
Creates a scalar array whose only item is R.

Z: Scalar array

 ⍴Z ←→ ⍳0
⍴⍴Z ←→ ,0")
      ("Monadic with axis" "Enclose with Axis" "Yields an array across the set of axes indicated by X"
       "Z←⊂[X]R
Yields an array whose items are the contiguous subarrays along
the set of axes indicated by X. That is, the set of axes indicated
by X is enclosed.

X: Simple scalar or vector, nonnegative integer.
   If X is nonempty, X∊⍳⍴⍴R.

Implicit argument: ⎕IO

 ⍴Z ←→ (⍴R)[(⍳⍴⍴R)~X] 
⍴↑Z ←→ (⍴R)[,X] 
⍴⍴Z ←→ (⍴⍴R)-⍴,X")
      ("Dyadic" "Partition" "Partitions R into an array of vectors specified by L"
       "Z←L⊂R
Partitions R into an array of vectors specified by L.

L: Simple scalar or vector of nonnegative integers
R: Nonscalar
Z: Array of vectors

 ⍴Z ←→ (¯1↓⍴R),+/2</0,L (after left scalar extended)
⍴⍴Z ←→ ⍴⍴R
 ≡Z ←→ 1+≡R")
      ("Dyadic with axis" "Partition with axis" "Partitions R into an array of vectors specified by L along axis X"
       "Z←L⊂[X]R
Partitions R into an array of vectors specified by L along axis
X.

L: Simple scalar or vector of nonnegative integers
R: Nonscalar
Z: Array of vectors
X: Simple scalar or one-item vector;
  nonnegative integer: X∊⍳⍴⍴R

Implicit argument: ⎕IO

X⊃⍴Z ←→ +/2</0,L
 ⍴⍴Z ←→ ⍴⍴R
  ≡Z ←→ 1+≡R"))
     t)

    ;; ========================================

    ("⊃"
     (("Monadic" "Disclose" "Structures the items of R into an array, whose rightmost axes come from the axes of the items of R"
       "Z←⊃R
Structures the items of R into an array, whose rightmost axes come
from the axes of the items of R.

  (⍴Z) ←→ (⍴R),↑⌈/(⍴¨̈(,R),⊂↑R)~⊂⍳0
 (⍴⍴Z) ←→ (⍴⍴R)+↑⌈/⍴¨̈⍴¨̈(,R),⊂↑R")
      ("Monadic with axis" "Disclose with axis" "Structures the items of R into an array. X defines the axes of Z, into which items of R are structured"
       "Z←⊃[X]R
Structures the items of R into an array. X defines the axes of Z,
into which items of R are structured.

X: Simple scalar or vector, nonnegative integers

Implicit argument: ⎕IO
 (⍴Z)[,X] ←→ ↑⌈/(⍴¨̈(,R),⊂↑R)~⊂ι0
      ⍴⍴Z ←→ (⍴⍴R)+⌈/∊⍴¨̈⍴¨̈(,R),⊂↑R")
      ("Dyadic" "Pick" "Selects an item of R as specified by the path indexes L"
       "Z←L⊃R
Selects an item of R as specified by the path indexes L.

L: Scalar or vector whose depth is ≤2; integer or empty

Implicit argument: ⎕IO

 ⍴Z ←→ Depends on the shape of the selected item
⍴⍴Z ←→ Depends on the rank of the selected item"))
     t)

    ;; ========================================

    ("∪"
     (("Monadic" "Unique" "Return an array of all unique elements in R")))

    ;; ========================================

    ("⍷"
     (("Dyadic" "Find" "Return a boolean array indicating the positions of the array L in R"
       "Z←L⍷R
Yields a Boolean array that maps to R. An item of Z is 1, where the
pattern L begins in the corresponding position of R. Otherwise, an
item of Z is 0.

Z: Simple Boolean array

Implicit argument: ⎕CT

 ⍴Z ←→ ⍴R
⍴⍴Z ←→ ⍴⍴R"))
     t)

    ;; ========================================

    ("≡"
     (("Monadic" "Depth" "Return the levels of nesting in R"
       "Z←≡R
Reports levels of nesting: 0 for a simple scalar; for other
arrays, 1 plus the depth of the item with the maximum depth.

Z: Simple scalar, nonnegative integers

 ⍴Z ←→ Empty
⍴⍴Z ←→ ,0")
      ("Dyadic" "Match" "Yields a 1 if the arguments are the same in structure and data, and a 0 otherwise"
       "Z←L≡R
Yields a 1 if the arguments are the same in structure and data, and
a 0 otherwise.

Z: Boolean

Implicit argument: ⎕CT
 ⍴Z ←→ ⍳0
⍴⍴Z ←→ ,0"))
     t)

    ;; ========================================

    ("⊥"
     (("Dyadic" "Decode" "Yields the values of array R evaluated in a number system with radices L"
       "Z←L⊥R
Yields the values of array R evaluated in a number system with
radices L.

L, R, and Z: Simple numeric array

⍴Z ←→ (1↓L),1↓⍴R
⍴⍴Z ←→ (0 1+⍴⍴L)+(0⌈¯1+⍴⍴R)"))
     t)

    ;; ========================================

    ("⊤"
     (("Dyadic" "Encode" "Yields the representation of R in the number system whose radices are L"
       "Z←L⊤R
Yields the representation of R in the number system whose radices
are L.

L, R, and Z: Simple numeric array

⍴Z ←→ (⍴L),⍴R
⍴⍴Z ←→ (⍴⍴L)+⍴⍴R"))
     t)
    (("~" "∼")
     (("Monadic" "Not" "Logical: ∼1 is 0, ∼0 is 1"))
     (("Dyadic" "Without" "Yields the items in L that do not occur in R"
       "Z←L~R
Yields the items in L that do not occur in R

L: Scalar or vector
Z: Vector

Implicit argument: ⎕CT
 ⍴Z ←→ Depends on the contents of L and R
⍴⍴Z ←→ ,1"))
     t)

    ;; ========================================

    ("⊢"
     ("Dyadic" "Right" "Returns R"
      "Z←L⊢R
Returns R (discarding L)")
     nil)

    ;; ========================================

    ("⊣"
     ("Dyadic" "Left" "Returns L"
      "Z←L⊢R
Returns L (discarding R)")
     nil)
    )
  "Documentation for APL symbols.")
