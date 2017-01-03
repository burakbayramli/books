function B = BASIS(A)
%BASIS	Basis for the column space.
%	B = BASIS(A) uses the pivot indices found by PLU(A)
%	to pick out a basis for the column space of A.
%
%	See also PLU, REF, SOLVE, NULL.

[P,L,U,pivcol] = plu(A);
rank = length(pivcol)
B = A(:,pivcol);
