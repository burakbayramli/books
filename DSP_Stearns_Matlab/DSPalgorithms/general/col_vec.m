function v=col_vec(A)
% v=col_vec(A)
%
% Converts any array A into column vector v.
% If A is Nx1, v=A. If A is 1xN, v=A.'.
% If A is a matrix, v=A scanned by columns.
% If A has dimension >2, pages are scanned sequentially.
% See also: row_vec
v=A(:);