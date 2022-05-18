%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function A=FEM_ConvecDiffAP1(eps,lambda,n)
%Matrix of the convection diffusion problem
%discretizd by  P1 Finite Element Method
%A is a n x n matrix 
%
h=1/(n+1);
B=2*diag(ones(n,1))-diag(ones(n-1,1),+1)-diag(ones(n-1,1),-1);
C=diag(ones(n-1,1),+1)-diag(ones(n-1,1),-1);
A=eps*B/h+lambda*C/2;


