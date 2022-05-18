%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function y=SPE_LegLinComb(x,c)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function y=SPE_LegLinComb(x,c)
%% Exercise 5.1
%% Evaluation of a linear combination of Legendre polynomials at
%% points x1,...xn
%%
%% input arguments 
%%       x: vector of components (x1<x2<...<xn)
%%       c: vector of components (c1,c2,...,cp), the coefficients
%%           of the linear combination
%% output argument 
%%       y: vector of components (y1,y2,...yn)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
n=length(x);  % number of points where the linear combination is computed
p=length(c);  % maximum degree of the polynomials on the linear combination 
if p>0,
  pol1=ones(1,n);
  y=c(1)*pol1;
else
  y=[];
end
if p>1,  
  pol2=x;	
  y=c(1)*pol1+c(2)*pol2;	
  for k=3:p
     pol=((2*k-3)*x.*pol2-(k-2)*pol1)/(k-1);
     y=y+c(k)*pol;
     pol1=pol2;
     pol2=pol;
  end
end
