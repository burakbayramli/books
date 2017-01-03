%nm131_2_2: nested structure computation may be critical
clear
for K=153:155
  lambda=100;
  S1=0; S2=0;
  for k=1:K
     p1=lambda^k/fctrl(k); S1=S1+p1;
     p2=lambda^k/fctrl(k); S2=S2+p2;
  end
  [S1 S2]*exp(-lambda)
end