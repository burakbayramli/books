%UNSTEADY_NAVIER unsteady flow problem in square domain
%   IFISS scriptfile: DJS; 20 September 2016, AR; 09 January 2019
% Copyright (c) 2009 D.J. Silvester
clear variables
global pde 
fprintf('Unsteady flow in a square domain ...\n')
global viscosity
viscosity=default('viscosity parameter (default 1/200)',1/200);
%
%% initialize for time stepping iteration: compute Stokes solution
%% load assembled matrices
gohome
cd datafiles
load square_stokes_nobc.mat
%
%
pde=14; domain=1;
%% set parameters
fprintf('Discrete Saddle-Point DAE system ...\n')
tfinal = default('target time? (default 1e8)',1e8);
tol = default('accuracy tolerance? (default 3e-5)',3e-5);
nonlin = default('number of Picard steps? (default 2)',2);
nstar = default('averaging frequency? (default 10)',10);
vswitch = default('plot vorticity evolution? 1/0',0);
dtzero=1e-9; uzero=zeros(size(f)); gzero=zeros(size(g));
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

save square_unsteadyflow.mat DT U Udot time viscosity
%
% visualise solution and hold the timestep sequence plot
marker = 'k.';
dttplot(DT(1:end-1),99,marker)
% compute mean quantities
[ke,acc,meanv] = energymeanvorticity(qmethod,mv,U,Udot,time,By,Bx,G,xy,1,100);
gohome, cd plotfiles
print('unsteadyflow_visc.eps','-depsc')
if vswitch==1
pause(3)
   if qmethod>1,
   square_unsteadyflowplot(qmethod,mv,U,time,By,Bx,G,xy,xyp,x,y,bound,0,167);
   else
   square_unsteadyflowplot(qmethod,ev,U,time,By,Bx,G,xy,xyp,x,y,bound,0,167);
   end
end
fprintf('\nTo generate snapshots of stationary streamlines\n')
fprintf('run <strong>square_unsteadyflowref</strong>\n')
%help square_unsteadyflowref
fprintf('To generate a streamline movie run <strong>square_flowmovie</strong>\n')
%help square_flowmovie

%%% compute divergence error
if qmethod>1
   error_div = q2div(xy,mv,[U(:,end);gzero]);	
else
   error_div = q1div(xy,ev,[U(:,end);gzero]);	 	
end
return
