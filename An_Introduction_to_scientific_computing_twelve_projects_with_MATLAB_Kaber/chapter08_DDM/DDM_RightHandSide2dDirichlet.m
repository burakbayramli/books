%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [Rhscol]=DDM_RightHandSide2dDirichlet(f2r,h,n1,n2,a1,a2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function [Smcol]=DDM_RightHandSide2dDirichlet(f2r,h,n1,n2,a1,a2)
%% Exercise 8.3
%% finite differences solution of the boundary conditions problem
%%  -nabla u=f  on [a1,b1]x[a2,b2]
%%  u(a1,x2)=f2(x2)          u(x1,a2)=f1(x1)
%%  u(b1,x2)=g2(x2)          u(x1,b2)=g1(x1)
%% Linear system right hand side  initialization with the right  hand side
%% function of the equation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Rhsm=zeros(n1,n2);
j=[1:n2];
for i=1:n1
   x1=a1+i*h;        
   Rhsm(i,j)=feval(f2r,x1,a2+j*h);
end
Rhscol=Rhsm(:);
