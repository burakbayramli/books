function [Qelem] = Qcalc(D,m,l);
% Purpose: Evaluate entries in the smoothness indicator for WENO
[x,w] = LegendreGQ(m); xq = x./2; Qelem = 0;
for i=1:m+1
    xvec = zeros(m-l+1,1);
    for k=0:m-l  xvec(k+1) = xq(i)^k./prod(1:k); end;
    Qelem = Qelem + (xvec'*D*xvec)*w(i)/2;
end
return
