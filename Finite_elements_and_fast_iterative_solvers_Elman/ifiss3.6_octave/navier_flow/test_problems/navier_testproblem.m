%NAVIER_TESTPROBLEM_UNIX    sets up reference Examples 8.1 to 8.4
%   IFISS scriptfile: DJS; 30 September 2016.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
gohome
clear variables
fprintf('\nspecification of reference Navier-Stokes problem.\n');
fprintf('\nchoose specific example (default is cavity)');
fprintf('\n     1  Channel domain');
fprintf('\n     2  Flow over a backward facing step');
fprintf('\n     3  Lid driven cavity');
fprintf('\n     4  Flow over a plate');
fprintf('\n     5  Flow over an obstacle');
fprintf('\n     6  Flow in a symmetric step channel\n');
sn = default('',3);
if sn==1,
system('/bin/cp ./stokes_flow/test_problems/poiseuille_flow.m ./stokes_flow/specific_flow.m');
system('/bin/cp ./stokes_flow/test_problems/poiseuille_bc.m ./stokes_flow/stream_bc.m');
   channel_stokes; solve_channel_navier;
elseif sn==2,
system('/bin/cp ./stokes_flow/test_problems/backwardstep_flow.m ./stokes_flow/specific_flow.m');
system('/bin/cp ./stokes_flow/test_problems/backwardstep_bc.m ./stokes_flow/stream_bc.m');
   longstep_stokes; solve_step_navier;
elseif sn==3,
   lid_model=default('cavity type leaky/tight/regularised 1/2/3 (regularised)',3); 
   if lid_model ==3,
system('/bin/cp ./stokes_flow/test_problems/regcavity_flow.m ./stokes_flow/specific_flow.m');
   elseif lid_model ==2,
system('/bin/cp ./stokes_flow/test_problems/tightcavity_flow.m ./stokes_flow/specific_flow.m');
   else
system('/bin/cp ./stokes_flow/test_problems/leakycavity_flow.m ./stokes_flow/specific_flow.m');
   end
system('/bin/cp ./stokes_flow/test_problems/zero_bc.m ./stokes_flow/stream_bc.m');
   square_stokes; solve_navier;
elseif sn==4,
system('/bin/cp ./stokes_flow/test_problems/plate_flow.m ./stokes_flow/specific_flow.m');
system('/bin/cp ./stokes_flow/test_problems/plate_bc.m ./stokes_flow/stream_bc.m');
   plate_stokes; solve_plate_navier;
elseif sn==5,
system('/bin/cp ./stokes_flow/test_problems/obstacle_flow.m ./stokes_flow/specific_flow.m');
system('/bin/cp ./stokes_flow/test_problems/obstacle_bc.m ./stokes_flow/stream_bc.m');
    obstacle_stokes; solve_obstacle_navier
elseif sn==6,
system('/bin/cp ./stokes_flow/test_problems/symstep_flow.m ./stokes_flow/specific_flow.m');
system('/bin/cp ./stokes_flow/test_problems/symstep_bc.m ./stokes_flow/stream_bc.m');
symstep_stokes; solve_step_navier
else
   error('reference problem datafile not found!');
end
