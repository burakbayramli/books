function H = permcols(H,p)
% syntax: H= permcols(H,p);
% permute columns of tableau H according to the permutation p

if nargin ~= 2
   error('must have 2 arguments');
end

[m,n] = size(H.val);
if length(p) ~= n 
  error('permutation not correct length');
end

H.val = H.val(:,p);
H.nonbas = H.nonbas(p);

if isfield(H,'dualbas')
  H.dualbas = H.dualbas(p);
end


tbl(H);

return;
