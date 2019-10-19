function H = addrow(H,x,lbl,r)
% syntax: H = addrow(H,x,lbl,r);
% add row x with label lbl as row r of the tableau H

if nargin ~= 4
   error('must have 4 arguments');
end
if length(lbl) > 3 
  error('label must have 3 or less characters');
end
if strmatch(lbl,H.nonbas,'exact') | strmatch(lbl,H.bas,'exact')
  error('label already in use');
end

[m,n] = size(H.val);
H.val = [H.val(1:r-1,:); x; H.val(r:m,:)];
H.bas = [H.bas(1:r-1); cellstr(lbl); H.bas(r:m)];
if ~isempty(H.obj)
  if H.obj >= r
    H.obj = H.obj + 1;
  end
end
if isfield(H,'dualnonbas')
  H.dualnonbas = [H.dualnonbas(1:r-1); cellstr(''); H.dualnonbas(r:m)];
end

tbl(H);

return;
