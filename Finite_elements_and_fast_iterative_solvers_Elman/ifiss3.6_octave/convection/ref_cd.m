%REF_CD set up problem in reference square domain 
%   IFISS scriptfile: DJS; 27 July 2015.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 

%% define geometry
pde=2; domain=1;
ref_domain(sn)
load ref_grid.mat
%
%% set up matrices
qmethod=1;
[ev,ebound] = q1grid(xy,mv,bound,mbound);
[A,N,Q,epe,eph,epw] = femq1_cd(xy,ev);
%
%% save resulting system
fprintf('system matrices saved in square_cd_nobc.mat ...\n')
gohome
cd datafiles
save square_cd_nobc.mat qmethod grid_type A N Q xy ev epe eph epw ebound bound outbc x y
