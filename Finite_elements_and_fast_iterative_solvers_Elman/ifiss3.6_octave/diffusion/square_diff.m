%SQUARE_DIFF solve Poisson problem in unit square domain 
%   IFISS scriptfile: DJS; 29 September 2013.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
clear variables
%% define geometry
pde=1; domain=1;
global viscosity
viscosity=1;
square_domain
load square_grid.mat
%
%% set up matrices
qmethod=default('Q1/Q2 approximation 1/2? (default Q1)',1);
% reference grid switch
if grid_type==1  % uniform grid
savesol=default('save results for reference 1/0 (yes/no)? (default no)',0);
else, savesol=0; end
if qmethod ==2, 
   [x,y,xy] = q2grid(x,y,xy,mv,bound);
   [A,M,f] = femq2_diff(xy,mv); 
   else
   [ev,ebound] = q1grid(xy,mv,bound,mbound);
   [A,M,f] = femq1_diff(xy,ev);
end 
%% boundary conditions
   [Agal,fgal] = nonzerobc(A,f,xy,bound);
%
%% save resulting system
fprintf('system saved in square_diff.mat ...\n')
gohome
cd datafiles
save square_diff.mat qmethod Agal M  fgal  xy x y 
%
%% compute solution
tic
fprintf('solving linear system ...  ')
x_gal=Agal\fgal;
fprintf('done\n')
etoc=toc; fprintf('Galerkin system solved in  %8.3e seconds\n\n',etoc) 
%
%
nmel=length(mv(:,1)); 
if savesol==0,
%% compute a posteriori error estimate and plot solution 
diffpost
   save square_diff.mat x_gal error_tot eex hx hy -append 
   if qmethod==1,
      errplot(x_gal,error_tot,ev,xy,x,y,11),
      save square_diff.mat ev ebound -append 
      elseif qmethod==2,   
      errplot2(x_gal,error_tot,mv,xy,x,y,12),
      save square_diff.mat mv mbound -append 
   end
else  
    solplot(x_gal,xy,x,y,13),
    xy_ref=xy; Aref=A; x_ref=x_gal;
    save refsolution.mat xy_ref Aref x_ref
    fprintf('solution data saved in refsolution.mat\n')
end