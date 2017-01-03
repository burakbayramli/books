%do_pade.m
%Pade approximation for f(x)=e^x
%Mc=[1 1 1/2 1/6 1/24 1/120];
f1=inline('exp(x)','x'); 
xo=0; 
M=3; N=2; %the degrees of Numerator/Denominator polynomial
[n,d]=padeap(f1,xo,M,N)
x0=-3.5; xf=0.5;
padeap(f1,xo,M,N,x0,xf); %to see the graphic results
%axis([x0 xf -1 1])
syms x
a=fliplr(sym2poly(taylor(exp(x),M+N+1,xo)));
[n,d]=padeap(a,xo,M,N)

pause
f31=inline('1./(1+8*x.^2)','x'); 
xo=0.5; %the center of Taylor expansion series
[n,d]=padeap(f31,xo,M,N) %to get the coefficients of numerator/denominator
a=fliplr(sym2poly(taylor(1/(1+8*(x+xo)^2),M+N+1)));
[n,d]=padeap(a,xo,M,N) %to get the coefficients of numerator/denominator
x0=0; xf=1; %left/right boundary of the interval
padeap('f31',xo,M,N,x0,xf); %to see the graphic results
