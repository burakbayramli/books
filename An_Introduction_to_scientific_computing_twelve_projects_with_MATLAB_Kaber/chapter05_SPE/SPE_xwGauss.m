%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [x,o]=SPE_xwGauss(s)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function [x,o]=SPE_xwGauss(s)
%% Exercise 5.2
%% Function to compute Gauss abscissa and weights of degree s
%% input argument
%%       s: degree of the integration quadrature
%% output arguments
%%       x: Gauss abscissa x1...xs
%%       o: Gauss weights o1...os
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% Computes the roots of polynomial  $L_s$
A=zeros(s,s);
for i=1:s-2
  A(i+1,i+2)=(i+1)/(2*i+1);
  A(i+1,i)=i/(2*i+1);
end
if s>1
 A(s,s-1)=(s-1)/(2*s-1);
 A(1,2)=1;
 x=eig(A);
 x=sort(real(x));
else 
  x=[0];
end
% Computes weights
p0=ones(s,1);
p1=x;
for j=2:s-1
    p2=(2*j-1)*x.*p1/j-(j-1)*p0/j;
    p0=p1;
    p1=p2;
end
if s==1
  o=[1];
elseif s==2;
  o=[0.5;0.5];
else
  o=2*(1.-x.^2)./(s*p2).^2;
end

