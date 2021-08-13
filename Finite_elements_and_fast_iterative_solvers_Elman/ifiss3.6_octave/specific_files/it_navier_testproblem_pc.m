%IT_NAVIER_TESTPROBLEM_PC    driver for iterative solution of Examples 8.2 to 8.3
%   IFISS scriptfile: HCE 4 June 2013, from DJS; 9 May 2012.
% Copyright (c) 2013 D.J. Silvester, H.C. Elman, A. Ramage
gohome
clear variables, clear functions
fprintf('\nspecification of reference Navier-Stokes problem.\n');
fprintf('\nchoose specific example (default is cavity)');
fprintf('\n     2  Flow over a backward facing step');
fprintf('\n     3  Lid driven cavity\n');
sn = default('',3);
if sn==2,
system('copy .\stokes_flow\test_problems\backwardstep_flow.m .\stokes_flow\specific_flow.m');
system('copy .\stokes_flow\test_problems\backwardstep_bc.m .\stokes_flow\stream_bc.m');
longstep_stokes; it_solve_step_navier;
elseif sn==3,
lid_model=default('cavity type leaky/tight/regularised 1/2/3 (regularised)',3);
if lid_model ==3,
system('copy .\stokes_flow\test_problems\regcavity_flow.m .\stokes_flow\specific_flow.m');
elseif lid_model ==2,
system('copy .\stokes_flow\test_problems\tightcavity_flow.m .\stokes_flow\specific_flow.m');
else
system('copy .\stokes_flow\test_problems\leakycavity_flow.m .\stokes_flow\specific_flow.m');
end
system('copy .\stokes_flow\test_problems\zero_bc.m .\stokes_flow\stream_bc.m');
square_stokes; it_solve_navier;
else
error('reference problem datafile not found!');
end
