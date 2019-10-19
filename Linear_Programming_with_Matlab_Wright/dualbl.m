function H = dualbl(H)
% syntax: H = dualbl(H);
% adds dual row and column labels in last two rows and columns

[m,n] = size(H.val);

if isempty(H.obj)
  H.dualbas = writelbl('v',1:n);
  H.dualnonbas = writelbl('u',1:m);
else
  if H.obj ~= m
    error('need to have objective row at end');
  end
  if strmatch('z',H.bas{H.obj},'exact')
    H.dualbas = writelbl('u',m:m+n-2,'w');
    H.dualnonbas = writelbl('u',1:m-1,'1');
  else
    H.dualbas = writelbl('x',m:m+n-2,'z');
    H.dualnonbas = writelbl('x',1:m-1,'1');
  end
end

tbl(H);

return;
