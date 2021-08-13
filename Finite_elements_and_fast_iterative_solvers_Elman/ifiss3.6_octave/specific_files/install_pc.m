%INSTALL_PC sets up IFISS on non-UNIX computer
%   IFISS scriptfile: DJS, HCE; 29 October 2013.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage (see readme.m)

if strncmp(computer,'MAC2',4)
   fprintf('\nInstalling MAC-OS specific files must be done manually:\n')
%
elseif strncmp(computer,'PCWIN',3)
   gohome;
   fprintf('\nInstalling PC-OS specific files.\n')
%
   delete .\diffusion\test_problems\diff_testproblem.m
system('copy .\specific_files\diff_testproblem_pc.m .\diffusion\test_problems\diff_testproblem.m');
%
   delete .\convection\test_problems\cd_testproblem.m
system('copy .\specific_files\cd_testproblem_pc.m .\convection\test_problems\cd_testproblem.m');
%
   delete .\stokes_flow\test_problems\stokes_testproblem.m
system('copy .\specific_files\stokes_testproblem_pc.m .\stokes_flow\test_problems\stokes_testproblem.m');
%
   delete .\navier_flow\test_problems\navier_testproblem.m
system('copy .\specific_files\navier_testproblem_pc.m .\navier_flow\test_problems\navier_testproblem.m');
%
  delete .\navier_flow\test_problems\it_navier_testproblem.m
system('copy .\specific_files\it_navier_testproblem_pc.m .\navier_flow\test_problems\it_navier_testproblem.m');
%
   delete .\timestepping\test_problems\heat_testproblem.m
system('copy .\specific_files\heat_testproblem_pc.m .\timestepping\test_problems\heat_testproblem.m');
%
   delete .\timestepping\test_problems\unsteady_cd_testproblem.m
system('copy .\specific_files\unsteady_cd_testproblem_pc.m .\timestepping\test_problems\unsteady_cd_testproblem.m');
%
   delete .\timestepping\test_problems\unsteady_navier_testproblem.m
system('copy .\specific_files\unsteady_navier_testproblem_pc.m .\timestepping\test_problems\unsteady_navier_testproblem.m');
%
   delete .\boussinesq_flow\test_problems\unsteady_bouss_testproblem.m
system('copy .\specific_files\unsteady_bouss_testproblem_pc.m .\boussinesq_flow\test_problems\unsteady_bouss_testproblem.m');

%
else
  fprintf('\nAll installed!\n')
end
