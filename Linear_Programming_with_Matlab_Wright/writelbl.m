function x = writelbl(label,range,extra)

n = length(range);
x=cell(n,1);
for i=1:n
  x{i,1} = sprintf('%s%d',label,range(i));
end
if nargin == 3
  if ischar(extra) 
    x{n+1,1} = extra;
  else
    error('must call with third argument a string');
  end
end

return;
