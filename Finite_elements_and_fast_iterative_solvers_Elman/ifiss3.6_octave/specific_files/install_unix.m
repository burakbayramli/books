%INSTALL_UNIX sets up IFISS on UNIX computer
%   IFISS scriptfile: DJS, HCE; 29 October 2013.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage (see readme.m)

if strncmp(computer,'MAC2',4)
   fprintf('\nInstalling MAC-OS specific files must be done manually:\n')
%
else
   gohome;
   fprintf('\nInstalling unix specific files.\n')
%
system('/bin/rm ./diffusion/test_problems/diff_testproblem.m');
system('/bin/cp ./specific_files/diff_testproblem_unix.m ./diffusion/test_problems/diff_testproblem.m');
%
system('/bin/rm ./convection/test_problems/cd_testproblem.m');
system('/bin/cp ./specific_files/cd_testproblem_unix.m ./convection/test_problems/cd_testproblem.m');
%
system('/bin/rm ./stokes_flow/test_problems/stokes_testproblem.m');
system('/bin/cp ./specific_files/stokes_testproblem_unix.m ./stokes_flow/test_problems/stokes_testproblem.m');
%
system('/bin/rm ./navier_flow/test_problems/navier_testproblem.m');
system('/bin/cp ./specific_files/navier_testproblem_unix.m ./navier_flow/test_problems/navier_testproblem.m');
%
system('/bin/rm ./navier_flow/test_problems/it_navier_testproblem.m');
system('/bin/cp ./specific_files/it_navier_testproblem_unix.m ./navier_flow/test_problems/it_navier_testproblem.m');
%
system('/bin/rm ./timestepping/test_problems/heat_testproblem.m');
system('/bin/cp ./specific_files/heat_testproblem_unix.m ./timestepping/test_problems/heat_testproblem.m');
%
system('/bin/rm ./timestepping/test_problems/unsteady_cd_testproblem.m');
system('/bin/cp ./specific_files/unsteady_cd_testproblem_unix.m ./timestepping/test_problems/unsteady_cd_testproblem.m');
%
system('/bin/rm ./timestepping/test_problems/unsteady_navier_testproblem.m');
system('/bin/cp ./specific_files/unsteady_navier_testproblem_unix.m ./timestepping/test_problems/unsteady_navier_testproblem.m');
%
system('/bin/rm ./boussinesq_flow/test_problems/unsteady_bouss_testproblem.m');
system('/bin/cp ./specific_files/unsteady_bouss_testproblem_unix.m ./boussinesq_flow/test_problems/unsteady_bouss_testproblem.m');
%
   fprintf('Done.\n')
end
