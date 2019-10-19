function H = relabel(H,varargin)
% syntax: H = relabel(H,old1,new1,old2,new2,...);
% relabel old as new

labels = (nargin-1)/2;
if nargin ~= 2*labels + 1
   error('must have odd number of arguments');
end
for i=1:labels
  H = actupdate(H,varargin{i*2-1:i*2});
end

tbl(H);

return;

function H = actupdate(H,old,new)

if length(old) > 3 
  error('old label must have 3 or less characters');
end
if length(new) > 3 
  error('new label must have 3 or less characters');
end
if strmatch(new,H.nonbas,'exact') 
  error('new label already in use (nonbasic)');
end
if strmatch(new,H.bas,'exact')
  error('new label already in use (basic)');
end

index = strmatch(old,H.nonbas,'exact');
if isempty(index) 
  index = strmatch(old,H.bas,'exact');
  if isempty(index) 
    error('no old label present');
  else
    H.bas(index) = cellstr(new);
  end
else
  H.nonbas(index) = cellstr(new);
end

