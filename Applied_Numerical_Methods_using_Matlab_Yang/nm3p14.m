%nm3p14.m
clear, clf
format long e
x=10^6*[1 1.1 1.2 1.3]; y=[1 2 5 10];
xi=x(1)+[0:1000]/1000*(x(end)-x(1));
[p,err,yi]=curve_fit(x,y,0,2,xi); p, err
subplot(220+iter), plot(x,y,'o',xi,yi), hold on
xmin=min(x); xmax=max(x);
x1= -2+4*(x-xmin)/(xmax-xmin);
x1i=??????????????????????????;
[p1,err,yi]=?????????????????????????; p1, err
plot(x,y,'o',xi,yi)
%To get the coefficients of the original fitting polynomial 
ps1=poly2sym(p1);
syms x; ps0=subs(ps1,x,-2+4/(xmax-xmin)*(x-xmin));
p0=sym2poly(ps0)
format short
