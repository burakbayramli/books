%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function f=DDM_g1Exact(x1)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function f=DDM_g1Exact(x1)
%% Exercise 8.3
%%  Finite differences resolution  of the boundary problem
%%  -nabla u=f  on [a1,b1]x[a2,b2]
%%  u(a1,x2)=f2(x2)          u(x1,a2)=f1(x1)
%%  u(b1,x2)=g2(x2)          u(x1,b2)=g1(x1)
%% Boundary condition on edge x2=b2 for the test case
%% with exact solution u(x1,x2)=sin(x1+x2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
global b2
f=sin(b2+x1);

