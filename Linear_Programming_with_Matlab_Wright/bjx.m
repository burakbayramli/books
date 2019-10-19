function A = bjx(A,R,S)
% syntax: B = bjx(A,R,S)
% input: tableau A, integer vectors R,S
% perform a block Jordan exchange with pivot A(R,S)

R = R(:); S = S(:);
[m,n] = size(A.val); 

% setdiff(1:m,R) := {1,...,m}\R
I = setdiff(1:m,R); J = setdiff(1:n,S);

% note that values are updated in place
% update pivot column
A.val(R,S) = inv(A.val(R,S)); 
A.val(I,S) = A.val(I,S)*A.val(R,S);

% update remainder of tableau
A.val(I,J) = A.val(I,J)-A.val(I,S)*A.val(R,J);

% update pivot row
A.val(R,J) = -A.val(R,S)*A.val(R,J);

% now update the labels
swap = A.bas(R);
A.bas(R) = A.nonbas(S);
A.nonbas(S) = swap;

if isfield(A,'dualbas')
  swap = A.dualbas(S);
  A.dualbas(S) = A.dualnonbas(R);
  A.dualnonbas(S) = swap;
end

tbl(A);

return;
