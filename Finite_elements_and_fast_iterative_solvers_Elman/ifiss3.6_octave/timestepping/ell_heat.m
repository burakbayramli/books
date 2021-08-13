%ELL_HEAT solve Heat Equation in L-shaped domain 
%   IFISS scriptfile: DJS; 27 May 2012. 
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
clear variables
%% define geometry
global pde 
pde=11; domain=2;
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
%% set parameters
fprintf('Discrete heat equation ODE system ...\n')
tfinal = default('target time? (default 100)',100);
tol = default('accuracy tolerance? (default 1e-4)',1e-4);
nstar = default('averaging frequency? (default 10)',10);
dtzero=1e-9; uzero=zeros(size(f));
%% compute solution
tic,
[DT,U,Udot,time] = stabtr(xy,bound,A,M,f,uzero,dtzero,tfinal,tol,nstar,0);
etoc=toc; fprintf('Integration took  %8.3e seconds\n\n',etoc) 
save ell_heat.mat DT U Udot time
%
% visualise solution and hold the timestep sequence plot
marker = default('plotting timestep sequence ... \nspecify plot symbol without quotes, default "bo"','bo');
dttplot(DT,99,marker)
pause(3)
ell_heatplot(U(:,2:end),time(2:end),xy,x,y,0.1,19);
