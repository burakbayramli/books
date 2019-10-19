function H = delcol(H,s)
% syntax: H = delcol(H,s);
% delete col numbered s (or labeled s) of the tableau H

if nargin ~= 2
   error('must have 2 arguments');
end

if ischar(s)
  s = strmatch(s,H.nonbas,'exact');
  if isempty(s) 
    error('no label matches this');
  end
end
H.val(:,s) = [];
H.nonbas(s) = [];
if isfield(H,'dualbas')
  H.dualbas(s) = [];
end

tbl(H);

return;
