%QUAD_DIFF solve Poisson problem in quadrilateral domain 
%   IFISS scriptfile: DJS; 26 May 2012. 
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
clear variables
%% define geometry
pde=1; domain=8;
global viscosity
viscosity=1;
quad_domain
load quad_grid.mat
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
fprintf('system saved in quad_diff.mat ...\n')
gohome
cd datafiles
save quad_diff.mat qmethod Agal M  fgal  xy x y 
%% compute solution
fprintf('solving linear system ...  ')
x_gal=Agal\fgal;
fprintf('done\n')
save quad_diff.mat x_gal  -append
% plot solution
if qmethod > 0 
   solplot(x_gal,xy,x,y,10);
   title(['Q',int2str(qmethod),' solution'])
   drawnow
end
