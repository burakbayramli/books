%HELPME_BOUSS Boussinesq flow problem interactive help
%   IFISS scriptfile: DJS;   1 October 2013.
% Copyright (c) 2012 D.J. Silvester, M.D. Mihajlovic.
fprintf(' \n');
fprintf('To setup and evolve a Boussinesq problem\n'); 
fprintf('simply run the driver: unsteady_bouss_testproblem\n\n');
help cavity_bouss
help step_bouss
fprintf(' (Type any character to continue.)\n\n')
pause;
fprintf('Nonzero temperature boundary conditions are set \n'); 
fprintf('in the user-defined function: /diffusion/specific_bc.m\n');
fprintf('Velocity boundary conditions are set \n'); 
fprintf('in the user-defined function: /stokes_flow/specific_flow.m\n');
fprintf(' \n');
fprintf('Checkpointing is performed every 200 timesteps (the parameter \n'); 
fprintf('<nnt> in the driver stabtrBouss) --- to continue time integration\n');
fprintf('from the latest checkpoint, simply run\n');
fprintf('restart_bouss or restart_step_bouss\n');
fprintf('To visualise the latest checkpoint solution\n'); 
fprintf('run the script file: ppbouss_checkpointdata\n\n');
fprintf('To test iterative solvers on the discrete system arising at any\n'); 
fprintf('specific time step, run the script file: snapshot_solvebouss\n');
fprintf(' \n');



