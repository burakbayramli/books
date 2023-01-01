%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function f=DDM_g1BB(x1)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function f=DDM_g1BB(x1)
%% Exercise 8.3
%%  Finite differences resolution of the boundary problem
%%  -nabla u=f  on [a1,b1]x[a2,b2]
%%  Dirichlet b.c. on edges //x1
%%  Fourier and Neumann b.c. on edges // x2
%%  du/dx1(a1,x2)+ca(u-uext)=0          u(x1,a2)=f1(x1)
%%  du/dx1(b1,x2)+cb(u-uext)=0          u(x1,b2)=g1(x1)
%% boundary condition on edge x2=b2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
f=40*ones(size(x1));

