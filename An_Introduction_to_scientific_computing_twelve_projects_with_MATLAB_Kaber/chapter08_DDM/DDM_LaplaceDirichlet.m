%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [mat]=DDM_LaplaceDirichlet(h,n1,n2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function [mat]=DDM_LaplaceDirichlet(h,n1,n2)
%% Exercise 8.2
%%  Finite differences resolution  of the boundary problem
%%  -nabla u=f  on [a1,b1]x[a2,b2]
%%  u(a1,x2)=f2(x2)          u(x1,a2)=f1(x1)
%%  u(b1,x2)=g2(x2)          u(x1,b2)=g1(x1)
%% Block building of the linear system matrix
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
L1=eye(n1,n1);  % block tridiagonal Laplacian matrix  
L2=toeplitz([-4 1 zeros(1,n1-2)]);  
mat=zeros(n1*n2,n1*n2);
for i=1:(n2-1)
   i1=(i-1)*n1;i2=i*n1;    %
   mat((i1+1):(i1+n1),(i1+1):(i1+n1)) = L2;  % diagonal block
   mat((i2+1):(i2+n1),(i1+1):(i1+n1)) = L1;   
   mat((i1+1):(i1+n1),(i2+1):(i2+n1)) = L1;   
end;
i1=(n2-1)*n1; 
mat((i1+1):(i1+n1),(i1+1):(i1+n1)) =L2;  % diagonal block
mat=mat/h^2;
