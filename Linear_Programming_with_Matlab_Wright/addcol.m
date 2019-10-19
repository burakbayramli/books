function H = addcol(H,x,lbl,s)
% syntax: H = addcol(H,x,lbl,s);
% add column x with label lbl as column s to the tableau H

if nargin ~= 4
   error('must have 4 arguments');
end
if length(lbl) > 3 
  error('label must have 3 or less characters');
end
if ~(isempty(strmatch(lbl,H.nonbas,'exact')) & isempty(strmatch(lbl,H.bas,'exact'))) 
  error('label already in use');
end

[m,n] = size(H.val);
H.val = [H.val(:,1:s-1) x H.val(:,s:n)];
H.nonbas = [H.nonbas(1:s-1); cellstr(lbl); H.nonbas(s:n)];
if isfield(H,'dualbas')
  H.dualbas = [H.dualbas(1:s-1); cellstr(''); H.dualbas(s:n)];
end

tbl(H);

return;
