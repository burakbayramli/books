%UNSTEADY_STEP_NAVIER solve Navier-Stokes problem in step domain
%   IFISS scriptfile: DJS; 22 August 2016, AR; 09 January 2019
% Copyright (c) 2009 D.J. Silvester
clear variables
global pde 
fprintf('Unsteady flow in a step domain ...\n')
global viscosity
viscosity=default('viscosity parameter (default 1/220)',1/220);
%
%% initialize for time stepping iteration: compute Stokes solution
%% load assembled matrices
gohome
cd datafiles
load step_stokes_nobc.mat
%
%
%% set parameters
pde=14; domain=3;
fprintf('Discrete Saddle-Point DAE system ...\n')
tfinal = default('target time? (default 1e8)',1e8);
tol = default('accuracy tolerance? (default 3e-5)',3e-5);
nonlin = default('number of Picard steps? (default 2)',2);
nstar = default('averaging frequency? (default 10)',10);
vswitch = default('plot solution evolution? 1/0',0);
dtzero=1e-9; uzero=zeros(size(f));gzero=zeros(size(g));
%% compute solution
tstart = tic;
if qmethod>1,
np=length(g);
AxB='defaultAxB';
[DT,U,Udot,time] = stabtrNS(qmethod,xy,mv,bound,A,B,sparse(np,np),G,AxB,...
                            uzero,dtzero,tfinal,tol,nstar,1,nonlin);
else
   error(['This functionality is not currently available for stabilized approximations.' ])
end
etoc=toc(tstart); fprintf('Integration took  %8.3e seconds\n\n',etoc)

save step_unsteadyflow.mat DT U Udot time viscosity


% visualise solution and hold the timestep sequence plot
marker = 'k.';
dttplot(DT(1:end-1),97,marker)
% compute mean quantities
[ke,acc,meanv] = energymeanvorticity(qmethod,mv,U,Udot,time,By,Bx,G,xy,1,99);
gohome, cd plotfiles
print('unsteadyflow_visc.eps','-depsc')

symm = default('Sanity check: is the step symmmetric? enter 0/1 (yes)',1);
if vswitch==1
     if qmethod>1,
   step_unsteadyflowplot(qmethod,mv,U,time,By,Bx,G,xy,xyp,x,y,bound,0.01,199,symm);
   else
   step_unsteadyflowplot(qmethod,ev,U,time,By,Bx,G,xy,xyp,x,y,bound,0.01,199,symm);
   end
end
fprintf('\nTo generate snapshots of stationary streamlines\n')
if symm, fprintf('run <strong>symstep_unsteadyflowref</strong>\n'),
fprintf('To generate a streamline movie run <strong>symstep_flowmovie</strong>\n')
else, fprintf('run <strong>step_unsteadyflowref</strong>\n'),
fprintf('To generate a streamline movie run <strong>step_flowmovie</strong>\n')
end


%%% compute divergence error
if qmethod>1
   error_div = q2div(xy,mv,[U(:,end);gzero]);	
else
   error_div = q1div(xy,ev,[U(:,end);gzero]);	 	
end
return
