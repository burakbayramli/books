function batchmode(testproblem)
%BATCHMODE enables batch processing for IFISS testproblem 
%   batchmode(testproblem);
%   input
%          testproblem  character string naming the testproblem
%                       must have the form "*_batch".m where "*" begins with
%                       "P"       for Poisson problems
%                       "CD"      for convection-diffusion problems
%                       "S"       for Stokes problems
%                       "NS"      for Navier-Stokes problems
%                       "T-NS"    for unsteady flow problems
%                       "B-NS"    for Boussinesq flow problems
%                       "itsolve" for iterative solution of linear systems
%                      "snapshot" for iterative solution of snapshot systems
%   side effect
%          If batchmode terminates prematurely because of an error or
%          execution of cntl-C, interactive input with IFISS may not
%          work correctly.  This is fixed by typing "activemode".
%
%
%   IFISS function: HCE, DJS; 27 May 2012.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 

global BATCH FID

% file containing input data 
batchfile=[testproblem,'_batch.m'];
[FID,message]=fopen(batchfile,'r');
% Error return on nonexistent or misnamed input file
if strcmp(message,'')~=1
   error(['INPUT FILE ERROR: ' message])
else
   disp(['Working in batch mode from data file ' batchfile])
end
if ~( strncmp(testproblem,'itsolve',7) | strncmp(testproblem,'snapshot',8) |...
      strncmp(testproblem,'P',1) | strncmp(testproblem,'CD',2) | ...
      strncmp(testproblem,'S',1) | strncmp(testproblem,'NS',2) | ...
	  strncmp(testproblem,'T-NS',4) |  strncmp(testproblem,'B-NS',4)),
    errmsg = 'INPUT FILE ERROR:\n';
    errmsg = [errmsg,'   Batch input filenames must have the form "*_batch.m"'];
    errmsg = [errmsg,' where "*" begins with\n'];
    errmsg = [errmsg,'   "P" for generation of Poisson problems\n'];
    errmsg = [errmsg,'   "CD" for generation of convection-diffusion problems\n'];
    errmsg = [errmsg,'   "S"  for generation of Stokes problems\n'];
    errmsg = [errmsg,'   "NS"  for generation of Navier-Stokes problems\n'];
    errmsg = [errmsg,'   "T-NS" for generation of unsteady flow problems\n'];
    errmsg = [errmsg,'   "B-NS"  for generation of Boussinesq flow problems\n'];
    errmsg = [errmsg,'   "itsolve" for iterative solution of linear systems.'];
    errmsg = [errmsg,'   "snapshot_solve" for iterative solution of snapshot systems.'];
    error('BATCH:err',errmsg);    
end  

% batch run
% switch to activate batch mode (off/on 0/1) (see "default.m")
BATCH=1;

% run appropriate driver
if strncmp(testproblem,'itsolve_stokes',14)
   load batchrun.mat
   itsolve_stokes
   gohome, cd datafiles
   save batchrun_itsolve.mat
elseif strncmp(testproblem,'itsolve',7)
   load batchrun.mat
   it_solve
   gohome, cd datafiles
   save batchrun_itsolve.mat
elseif strncmp(testproblem,'snapshot_flow',13)
   load unsteadyrun.mat
   snapshot_solveflow
elseif strncmp(testproblem,'snapshot_bouss',14)
   load unsteadyrun.mat
   snapshot_solvebouss
else 
   if strncmp(testproblem,'P',1)  
      diff_testproblem
   gohome, cd datafiles, save batchrun.mat
   elseif strncmp(testproblem,'CD',2)
      cd_testproblem
   gohome, cd datafiles, save batchrun.mat
   elseif strncmp(testproblem,'S',1)
      stokes_testproblem   
    gohome, cd datafiles, save batchrun.mat
   elseif strncmp(testproblem,'NS',2)
      navier_testproblem
   gohome, cd datafiles, save batchrun.mat
   elseif strncmp(testproblem,'T-NS',4)
   unsteady_navier_testproblem
   gohome, cd datafiles, save unsteadyrun.mat  
   elseif strncmp(testproblem,'B-NS',4)
   unsteady_bouss_testproblem 
   gohome, cd datafiles, save unsteadyrun.mat
   end
end

% switch back to interactive mode
activemode
return
