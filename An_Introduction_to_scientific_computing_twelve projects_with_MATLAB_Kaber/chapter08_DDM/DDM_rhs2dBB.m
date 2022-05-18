%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function y=DDM_rhs2dBB(x1,x2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function f=DDM_rhs2dBB(x1)
%% Exercise 8.3
%% finite differences solution of the boundary conditions problem
%%  -nabla u=f/k  on [a1,b1]x[a2,b2]
%% 	 Dirichlet b.c. on edges //x1
%%   Fourier Neumann b.c. on edges  // x2
%%  du/dx1(a1,x2)+ca(u-uext)=0          u(x1,a2)=f1(x1)
%%  du/dx1(b1,x2)+cb(u-uext)=0          u(x1,b2)=g1(x1)
%% Right hand side  f(x1,x2)/k uniform heat source in the bus bar 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
y=0.5*ones(length(x1),1);

