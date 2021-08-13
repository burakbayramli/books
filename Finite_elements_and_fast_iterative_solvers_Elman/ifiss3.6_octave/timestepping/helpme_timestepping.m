%HELPME_TIMESTEPPING unsteady problem solution interactive help 
%   IFISS scriptfile: DJS; 30 December 2009. 
% Copyright (c) 2009 D.J. Silvester

fprintf(' \n');
fprintf(' To generate and solve the heat equation over a\n');
fprintf(' rectangular-shaped domain, run the script file: box_heat \n');
fprintf(' For an L-shaped domain run the script file: ell_heat\n');
fprintf(' \n');
fprintf(' Alternatively, the driver  heat_testproblem  offers a choice\n');
fprintf(' of test problems\n');
fprintf(' \n');
fprintf(' Nonzero boundary conditions are set in the user-defined\n');
fprintf(' function: /diffusion/specific_rhs.m\n'); 
fprintf(' \n');
fprintf(' \n');
fprintf(' To solve an unsteady convection-diffusion problem with a specified\n');
fprintf(' viscosity parameter run the script file: unsteady_cd\n');
fprintf(' Nonzero boundary conditions are again set in the function\n');
fprintf(' /diffusion/specific_bc.m\n');
fprintf(' \n');
fprintf(' Alternatively, simply run the driver: unsteady_cd_testproblem\n\n');
help unsteady_cd_testproblem
fprintf(' \n');
fprintf(' To solve an unsteady flow problem in a square cavity with a specified\n');
fprintf(' viscosity parameter run the script file: unsteady_navier\n');
fprintf(' For an Backward step domain run the script file: unsteady_step_navier\n');
fprintf(' Nonzero boundary conditions are set in the function\n');
fprintf(' /stokes_flow/specific_flow.m\n');
fprintf(' \n');
fprintf(' Alternatively, simply run the driver: unsteady_navier_testproblem\n');
fprintf(' \n');
help unsteady_navier_testproblem
