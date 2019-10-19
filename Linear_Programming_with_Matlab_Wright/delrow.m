function H = delrow(H,r)
% syntax: H = delrow(H,r);
% delete row numbered r (or labeled r) of the tableau H

if nargin ~= 2
   error('must have 2 arguments');
end

if ischar(r) 
  r = strmatch(r,H.bas,'exact');
  if isempty(r)
    error('no label maches this');
  end
end
H.val(r,:) = [];
H.bas(r) = [];
H.obj = setdiff(H.obj,r);
if H.obj > r
  H.obj = H.obj - 1;
end
if isfield(H,'dualbas')
  H.dualnonbas(r) = [];
end

tbl(H);

return;
