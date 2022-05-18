%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [x,y,err]=SPE_CalLegExp(s,P,npt,Test)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function [x,y,err]=SPE_CalLegExp(s,P,npt,Test)
%% Exercise 5.3
%% Compute and displays Legendre expansion of a function
%%
%% Input arguments:
%%      s: degree of Gauss quadrature used to compute the 
%%                    coefficients of the expansion
%%      P: degree of the expansion
%%    npt: number of points to evaluate the expansion in
%%          the interval   [-1,1]
%%   Test: function whose  serie is computed
%% Output arguments:
%%      x: points the expansion is evaluated
%%      y: values of the expansion at points x
%%    err: error in supremum norm between the function 
%%          and its expansion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% computes degree s Gauss integration 
%% weights and abscissa
[absc,weights]=SPE_xwGauss(s);
t=feval(Test,absc);
%
% computes Legendre polynomials at points absc, in arrays LX0,
% LX1 et LX2.
% computes expansion coefficients  in array  c
u=t.*weights; 
LX0=ones(s,1);
LX1=absc;
c=zeros(P+1,1);
c(1)=t'*weights/2;
c(2)=3*u'*LX1/2;
for k=3:P+1  
  % computes $L_{k-1}$  values at points of integration
  % nL_n(absc)=(2n-1).absc.L_{n-1}(absc) -(n-1)L_{n-2}(absc)
  % Beware of index shift in MATLAB
    LX2=((2*k-3)*absc.*LX1-(k-2)*LX0)/(k-1);
  % computes f_{k-1} in c(k)
    c(k)=(2*k-1)*u'*LX2/2;
  % verifies that polynomials are orthogonal:
  %  fprintf('(LX2.*LX1)*weight = %f\n', (LX2.*LX1)'*weights);
  %  fprintf('(LX2.*LX0)*weight = %f\n', (LX2.*LX0)'*weights);
    LX0=LX1;
    LX1=LX2;   
end
%
% computes expansion at npt points in interval [-1,1]
x=linspace(-1,1,npt);
y=SPE_LegLinComb(x,c);
% error between Legendre serie and function
err=norm(Test(x)-y,inf); 
