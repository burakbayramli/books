
%Determine machine epsilon in matlab
%
eps=1
while (eps+1>1)
  eps=eps/2;
end
eps*2