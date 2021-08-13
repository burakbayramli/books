%RESTART_OBSTACLE_NAVIER restart unsteady flow in obstacle domain
%   s-IFISS scriptfile: DJS; 20 September 2016.
% Copyright (c) 2014 D.J. Silvester, H.C. Elman
clear all
global pde vscosity

fprintf('Navier-Stokes flow in an obstacle domain ... \n')
% load data from original integration
gohome, cd datafiles
load obstacle_unsteadyflow.mat
fprintf('viscosity parameter is  %11.3e\n',viscosity)
tbegin=time(end); dt0=DT(end-1); dt=DT(end);
u=U(:,end); ub=U(:,end-1);
udot=Udot(:,end); udotb=Udot(:,end-1);
n=length(DT)-1; oldDT=DT(1:n);

% load assembled matrices
gohome, cd datafiles
load obstacle_stokes_nobc.mat

% initialize for time stepping iteration
fprintf('restarting from %11.3e seconds\n',tbegin)
%% set parameters
pde=14; domain=3;
initdata=struct('u',u,'ub',ub,'udot',udot,'udotb',udotb, ...
                'dt0',dt0,'dt',dt,'time',tbegin,'n',n);
tfinal = default('new final time? (1e14)',1e14);
tol = default('accuracy tolerance? (default 3e-5)',3e-5);
nonlin = default('number of Picard steps? (default 1)',1);
nstar = default('averaging frequency? (default 10)',10);
gzero=zeros(size(g));
%% compute solution
np=length(g);
AxB='defaultAxB';
[DT,U,Udot,xtime] = restart_stabtrNS(nonlin,qmethod,xy,mv,bound,A,B,sparse(np,np),...
                                     G,AxB,initdata,tfinal,tol,nstar,1);
% visualise solution and hold the timestep sequence plot
refDT=[oldDT,DT];
marker = 'ro'; dttplot(refDT,99,marker)

Ufinal=U(:,end);
[ke,acc,meanv] = energymeanvorticity(qmethod,mv,U,Udot,xtime,By,Bx,G,xy,1,101);
