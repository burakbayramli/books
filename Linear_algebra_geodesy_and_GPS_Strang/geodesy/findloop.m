function N = findloop(A)
%FINDLOOP  Nullspace of a matrix
%     	  N = F_LOOP(A) uses the pivoting LU factorization computed by
%	        PLU and the resulting reduced row echelon form computed by REF to
%	        find a matrix N whose columns are a basis for the nullspace of A.
%	        The number of columns of N is the nullity of A.
%	        If A has independent columns, then N is empty, N = [];

% This is a copy of null.m from Strang's toolbox for Linear Algebra

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
%%%%%%%%%%%%% end findloop.m  %%%%%%%%%%%%%%%%%
