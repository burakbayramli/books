%UNSTEADY_STEP_BOUSS solve Boussinesq flow problem in step domain
% for further details see
%  "David Silvester, Alex Bespalov and Catherine Powell",
%  "A framework for the development of implicit solvers
%     for incompressible flow problems",
%  "Discrete and Continuous Dynamical Systems --- Series S",
%  "September 2012, to appear". 
%  "http://eprints.ma.man.ac.uk/1724/"

%   IFISS scriptfile: DJS;  13 May 2012.
% Copyright (c) 2012 D.J. Silvester, M.D. Mihajlovic.

clear variables
global Ra Pr H L
fprintf('\n\nUnsteady Boussinesq flow in a backward step domain ...\n')
fprintf('To restart integration from Checkpoint datafile run restart_bouss\n')
Ra=default('Rayleigh number (default 17750)',17750);
Pr=default('Prandtl number (default 7.1)',7.1);
%
% initialize for time stepping iteration: load assembled matrices
gohome; 
cd datafiles; 
load step_bouss_nobc.mat; load step_grid1h.mat 
%
% set parameters
tfinal = default('target time? (default 100)',100);
nmax = default('number of timesteps (x200)? (default 5)',5); %% 5 | 1000 timesteps
tol = default('accuracy tolerance? (default 3e-5)',3e-5);
nonlin = default('number of nonlinear Picard correction steps? (default is none)',0);
nstar = default('averaging frequency? (default 10)',10);
vswitch = default('plot solution evolution? 1/0',0);
xout = default('generate solution history data points? 1/0 (default is no)',0);
tout = xout+1; 
dtzero=1e-9;
%

% unpack grid data
xyv=grid(1).xyv;  [nnv,dd]=size(xyv(:,1));
xyp=grid(1).xyp;  [nnp,dd]=size(xyp(:,1));
xyt=grid(1).xyt;  [nnt,dd]=size(xyt(:,1));
uzero=zeros(2*nnv,1); gzero=zeros(nnp,1); hzero=zeros(nnt,1);
%
% pack start data
initdata=struct('uzero',uzero,'ttzero',hzero, ...
                'dtzero',dtzero,'restart',0); 
%
% compute solution
tic,
if qmethod ==12,
[refDT,U,Udot,T,Tdot,reftime] = ...
    stabtrBouss(nonlin,qmethod,grid,spmat,initdata,tfinal,tol,nstar,nmax,tout);
else
error('Requested mixed approximation is not yet implemented!')
end
etoc=toc; fprintf('integration took  %8.3e seconds\n\n',etoc) 
%
% update the problem definition in the final solution file 
save('stabtrBouss_end.mat','pde','domain','-append');
% and the checkpoint file (if it exists)
% update the time history in the checkpoint file if it exists
if exist('stabtrBouss_restart.mat','file')==2,
save('stabtrBouss_restart.mat','pde','domain','-append'); end
%
% update the timestep sequence and plot
fprintf('Timestep history ... ') 
marker = 'bo';
dttplot(refDT,199,marker)
pause(3)
%
% visualise solution 
[ndof,nsaved]=size(U); ltt=length(reftime);
ttt=[ltt-nsaved+1:ltt]; stt=[1:nsaved]; 
soltime=reftime(ttt); %%% list of saved solution times
solDT=refDT(ttt);     %%% list of saved solution timesteps
tout=0; unpack_boussdata
if vswitch==1
fprintf('\nMovies ...') 
bouss_solplot(domain,U(:,stt),T(:,stt),soltime,...
			  xyv,xyt,grid(1).x,grid(1).y,L,H,0,.1,200)
end
%
%plot the final solution
fprintf('\nfinal time solution ...\n')
snaptime=[nsaved];
step_unsteadytempref(T,soltime,xyt,grid(1).x,grid(1).y,snaptime)
step_unsteadyflowref(2,mv2,U,soltime,Av,...
		     BBy,BBx,Qv,xyv,xyp,grid(1).x,grid(1).y,bnd_d,snaptime);  
%%% compute the divergence error
error_div = q2div(xyv,grid(1).mv2,[U(:,end);gzero]);	
return
