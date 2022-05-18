%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function y=APP_spline0()
n0=10;E=[];N=[];
for i=1:10, 
    n=i*n0;E=[E;errorS0(n)];N=[N;n];
end;
loglog(N,E,'-+','MarkerSize',10,'LineWidth',3);
set(gca,'FontSize',24);
xlabel('log n');ylabel('log Error');
fprintf('slope of the straight line  = %g\n ',log(E(end)/(E(1)))/log(N(end)/N(1)));
title('piecewise constant approximation')
function y=errorS0(n)
x=(0:n)'/n;h=1/n;fx=f(x);
%Evaluation of $p_i$ on each interval $[x_i,x_{i+1}]$
y=[];
for i=1:n
   Ii=linspace(x(i),x(i+1),20);
   fi=f(Ii);
   Si=f(.5*(x(i)+x(i+1)));
   y=[y norm(Si-fi,'inf')];   
end
y=max(y);


function y=f(x)
y=sin(4*pi*x);
