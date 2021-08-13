%ELL_DIFF solve Poisson problem in L-shaped domain 
%   IFISS scriptfile: DJS; 29 September 2013.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
clear variables
%% define geometry
pde=1; domain=2;
global viscosity
viscosity=1;
ell_domain
load ell_grid.mat
%
%% set up matrices
qmethod=default('Q1/Q2 approximation 1/2? (default Q1)',1);
if qmethod ==2, 
   [x,y,xy] = q2grid(x,y,xy,mv,bound);
   [A,M,f] = femq2_diff(xy,mv); 
else
   [ev,ebound] = q1grid(xy,mv,bound,mbound);
   [A,M,f] = femq1_diff(xy,ev);
end 
%
%% boundary conditions
[Agal,fgal] = nonzerobc(A,f,xy,bound);
%
%% save resulting system
fprintf('system saved in ell_diff.mat ...\n')
gohome
cd datafiles
save ell_diff.mat qmethod Agal M  fgal  xy x y 
%% compute solution
tic, fprintf('solving linear system ...  ')
x_gal=Agal\fgal;
fprintf('done\n')
etoc=toc; fprintf('Galerkin system solved in  %8.3e seconds\n\n',etoc) 

%
%% compute a posteriori error estimate and plot solution
diffpost
save ell_diff.mat x_gal error_tot eex hx hy -append 
if qmethod ==1,
    errplotl(x_gal,error_tot,ev,xy,x,y,11),
    save ell_diff.mat ev ebound -append 
elseif qmethod==2,   
    errplotl2(x_gal,error_tot,mv,xy,x,y,12),
    save ell_diff.mat mv mbound -append 
end
