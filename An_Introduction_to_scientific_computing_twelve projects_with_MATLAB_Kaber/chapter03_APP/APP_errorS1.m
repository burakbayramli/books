%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function y=APP_errorS1(n)
x=(0:n)'/n;h=1/n;fx=f(x);
%Evaluation of $p_i$ on each interval $[x_i,x_{i+1}]$
y=[];
for i=1:n
   a=(fx(i+1)-fx(i))/h;
   Ii=linspace(x(i),x(i+1),20);
   fi=f(Ii);
   Si=a*(Ii-x(i))+fx(i);
   y=[y norm(Si-fi,'inf')];
end
y=max(y);

function y=f(x)
y=sin(4*pi*x);
