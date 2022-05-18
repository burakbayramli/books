%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [mat]=DDM_LaplaceFourier(h,n1,n2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function [mat]=DDM_LaplaceFourier(h,n1,n2)
%% Exercise 8.3
%%  Finite differences resolution of the boundary problem
%%  -nabla u=f  on [a1,b1]x[a2,b2]
%%  Dirichlet b.c. on edges //x1
%%  Fourier and Neumann b.c. on edges // x2
%%  du/dx1(a1,x2)+ca(u-uext)=0          u(x1,a2)=f1(x1)
%%  du/dx1(b1,x2)+cb(u-uext)=0          u(x1,b2)=g1(x1)
%% Block building of the linear system matrix
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cth=0.75*h;
n11=n1+2;
L1=eye(n11,n11);  %block tidiagonal laplacian matrix 
L2=toeplitz([-4 1 zeros(1,n11-2)]);       
L2(n11,n11)=-3;         % Neumann b. c. at x1=b1
L2(1,1)=-3+cth;         % Fourier b. c. at  x1=a1
mat=zeros(n11*n2,n11*n2);
for i=1:(n2-1)
   i1=(i-1)*(n11);i2=i*(n11);    %
   mat((i1+1):(i1+n11),(i1+1):(i1+n11)) = L2;  % diagonal block
   mat((i2+1):(i2+n11),(i1+1):(i1+n11)) = L1;   
   mat((i1+1):(i1+n11),(i2+1):(i2+n11)) = L1;   
end;
i1=(n2-1)*n11; 
mat((i1+1):(i1+n11),(i1+1):(i1+n11)) =L2;  % diagonal block
mat=mat/h^2;
