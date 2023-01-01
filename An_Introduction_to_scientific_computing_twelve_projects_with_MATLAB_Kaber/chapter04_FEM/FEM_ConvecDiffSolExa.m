%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [y,yp]=FEM_ConvecDiffSolExa(eps,lambda,fc,x)
%solution of convection diffusion problem
%eps, lambda and fc are constant
y =fc/lambda*(x-(1-exp(lambda*x/eps))./(1-exp(lambda/eps)));
%computation of the derivative
yp=fc/lambda*(1+lambda/eps*exp(lambda*x/eps)./(1-exp(lambda/eps)));
