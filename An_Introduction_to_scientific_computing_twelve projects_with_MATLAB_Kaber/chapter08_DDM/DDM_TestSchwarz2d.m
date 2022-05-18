%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exercise 8.4
%% Schwarz  method of domain decomposition with overlapping for the finite differences
%% solution of the boundary conditions problem
%%  -nabla u=f  on [a1,b1]x[a2,b2]
%%  Test cases 1 and 2: Dirichlet boundary conditions
%%  u(a1,x2)=f2(x2)          u(x1,a2)=f1(x1)
%%  u(b1,x2)=g2(x2)          u(x1,b2)=g1(x1) 
%%  Test case 3: Fourier and Neumann boundary conditions on edges // x2
%%  du/dx1(a1,x2)+ca(u-uext)=0          u(x1,a2)=f1(x1)
%%  du/dx1(b1,x2)+cb(u-uext)=0          u(x1,b2)=g1(x1)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fprintf('\n\n\n Schwarz DDM with overlapping in  2D\n');
clear
close all
% size of global domain (b2 contraint may not be exactly obtained)
global a1
global a2
global b1
global b2
fprintf(' Dirichlet boundary conditions on the 4 edges\n');
fprintf(' Test case with a known solution \n');
a1=0; a2=0;  b1=1; b2=30;    
n1=10;  % number of points in x1 direction 
ns=20;   % number of subdomains
no=10;   % number of points in each overlapping region in the x2 direction 

DDM_Schwarz2dDirichlet(n1,ns,no,a1, a2,b1,b2,'DDM_rhs2dExact','DDM_f1Exact','DDM_g1Exact','DDM_f2Exact','DDM_g2Exact',1);
fprintf('Hit any key to go on\n');pause;
%
fprintf('Thermal shock test case\n');
%
b1=6; b2=20;
ns=4;
DDM_Schwarz2dDirichlet(n1,ns,no,a1, a2,b1,b2,'DDM_rhs2dCT','DDM_f1CT','DDM_g1CT','DDM_f2CT','DDM_f2CT',1);
fprintf('Hit any key to go on\n');pause;
%
fprintf(' Fourier and Neumann boundary conditions on edges // x2. \n')
fprintf('Modeling of temperature in a bus bar\n');
%
a1=0;  a2=0; b1=6; b2=20;    
ns=3;  no=5;
DDM_Schwarz2dFourier(n1,ns,no,a1, a2,b1,b2,'DDM_rhs2dBB','DDM_f1BB','DDM_g1BB',1);
