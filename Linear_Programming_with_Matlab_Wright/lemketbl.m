function H = lemketbl(M,q)
% syntax: H = lemketbl(M,q);
% generate labeled Lemke tableau H from square matrix M and vector q

if nargin ~= 2
  error('require two arguments for lemketbl');
end
[m,n] = size(M);
q = q(:);
if m ~= n
  error('first argument must be a square matrix');
elseif length(q) ~= m
  error('second argument must be a column vector');
end

H.val = [M q];
H.bas = writelbl('w',1:m); 
H.nonbas = writelbl('z',1:n,'1');
H.obj = [];

tbl(H);

return; 
