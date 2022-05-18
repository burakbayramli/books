%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function A=FEM_ConvecDiffAP2(eps,lambda,n)
%Matrix of the convection diffusion problem
%discretizd by a P2 Finite Element Method
  %A is a (2n+1)x(2n+1) matrix 
%
h=1/(n+1);
T1(1:2:2*n+1)=16*ones(1,n+1);T1(2:2:2*n)=14*ones(1,n);
T2(1:2:2*n-1)=zeros(1,n);T2(2:2:2*n-2)=ones(1,n-1);
B=-8*diag(ones(2*n,1),1)+diag(T2,2);
B=B+B'+diag(T1);
T2(1:2:2*n-1)=zeros(1,n);T2(2:2:2*n-2)=.5*ones(1,n-1);
C=-2*diag(ones(2*n,1),1)+diag(T2,2);
C=C-C';
A=(eps*B/h-lambda*C)/3;


