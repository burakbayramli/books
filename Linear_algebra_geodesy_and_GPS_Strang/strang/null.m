function N = null(A)
%NULL	Nullspace of a matrix
%	N = NULL(A) uses the pivoting LU factorization computed by
%	PLU and the resulting reduced row echelon form computed by REF to
%	find a matrix N whose columns are a basis for the nullspace of A.
%	The number of columns of N is the nullity of A.
%	If A has independent columns, then N is empty, N = [];
%
%	(This supersedes the MATLAB function NULL(A) which computes a
%	basis for the nullspace of A with orthonormal columns and, for
%	badly conditioned problems, even a possibly different dimension.)
%
%	See also PLU, REF, SOLVE.

[R,pivcol] = ref(A);
[m,n] = size(A);

% The rank is the number of pivot columns.
r = length(pivcol);

% The nonpivot columns of R provide a basis for the nullspace.

nopiv = 1:n;
nopiv(pivcol) = [];
N = zeros(n,n-r);
if n > r
   N(nopiv,:) = eye(n-r,n-r);
   if r > 0
      N(pivcol,:) = -R(1:r,nopiv);
   end
end
