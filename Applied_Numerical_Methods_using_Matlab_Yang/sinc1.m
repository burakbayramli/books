function x=sinc1(t,D)
if nargin<2, D=1; end
t(find(t==0))=eps;
x=sin(pi*t/D)./(pi*t/D);
  