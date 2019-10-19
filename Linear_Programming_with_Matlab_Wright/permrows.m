function H = permrows(H,p)
% syntax: H= permrows(H,p);
% permute rows of tableau H according to the permutation p

if nargin ~= 2
   error('must have 2 arguments');
end

[m,n] = size(H.val);
if length(p) ~= m 
  error('permutation not correct length');
end

H.val = H.val(p,:);
if ~isempty(H.obj)
  H.obj = find(p==H.obj);
end
H.bas = H.bas(p);

if isfield(H,'dualbas')
  H.dualnonbas = H.dualnonbas(p);
end


tbl(H);

return;
