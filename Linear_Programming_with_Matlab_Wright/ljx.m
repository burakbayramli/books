function A = ljx(A,r,s)
% syntax: B = ljx(A,r,s);
% input: labeled tableau A, integers r,s
% perform a labeled Jordan exchange with pivot A(r,s)

A.val = jx(A.val,r,s);

% now update the labels
[m,n] = size(A.val);
swap = A.bas(r);
A.bas(r) = A.nonbas(s);
A.nonbas(s) = swap;

if isfield(A,'dualbas')
  swap = A.dualbas(s);
  A.dualbas(s) = A.dualnonbas(r);
  A.dualnonbas(r) = swap;
end

tbl(A);

return;
