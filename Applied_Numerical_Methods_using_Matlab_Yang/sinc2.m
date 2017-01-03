function x=sinc2(t)
global D
t(find(t==0))=eps;
x=sin(pi*t/D)./(pi*t/D);
  
  