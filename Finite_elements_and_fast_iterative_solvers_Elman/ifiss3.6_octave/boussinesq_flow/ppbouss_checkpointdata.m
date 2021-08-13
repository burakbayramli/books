%PPBOUSS_CHECKPOINTDATA postprocesses Boussinesq checkpoint data
%   IFISS scriptfile: DJS;  27 May 2012.
% Copyright (c) 2012 D.J. Silvester, M.D. Mihajlovic.
clear variables
fprintf('\n\nUnsteady Boussinesq flow postpocessing.\n')

% load checkpoint data
fprintf('loading data from Checkpoint datafile ...\n')
gohome; 
cd datafiles; 
load stabtrBouss_restart.mat

%% load grid data and discrete problem matrices
if domain==3,
fprintf('STEP domain flow problem \n')
load step_bouss_nobc.mat; load step_grid1h.mat 
hty=0;
elseif domain==7,
fprintf('CAVITY domain flow problem\n')
load rect_bouss_nobc.mat; load rect_grid1h.mat 
end

% unpack grid data
xyv=grid(1).xyv;  [nnv,dd]=size(xyv(:,1));
xyp=grid(1).xyp;  [nnp,dd]=size(xyp(:,1));
xyt=grid(1).xyt;  [nnt,dd]=size(xyt(:,1));
tout=0; unpack_boussdata

fprintf('Movies ...')
nstep=length(soltime)
bouss_solplot(domain,U(:,5:5:nstep),T(:,5:5:nstep),soltime(5:5:nstep),...
			  xyv,xyt,grid(1).x,grid(1).y,L,H,hty,.1,200);
fprintf('To expore solvers run snapshot_solvebouss...\n\n')
