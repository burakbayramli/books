%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
n=10;
x=(0:n)'/n;
g=0:0.01:1;
test1=inline('sin(10.*x.*cos(x))')
c=APP_dd(x);
y=APP_interpole(c,x,g);
yg=test1(g);
plot(g,yg,g,y,'r+')
hold on;
yx=test1(x);
plot(x,yx,'O');
hold off
title(['Lagrange interpolation of function sin(10.*x.*cos(x))';...
       '                     with divided differences        '])
legend('f','I_nf','xy_i')


