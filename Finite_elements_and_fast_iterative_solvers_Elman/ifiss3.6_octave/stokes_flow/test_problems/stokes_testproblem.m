%STOKES_TESTPROBLEM_UNIX sets up Examples 3.1 to 3.4
%   IFISS scriptfile: DJS; 9 January 2019.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
gohome
clear variables
fprintf('\nspecification of reference Stokes problem.\n')
fprintf('\nchoose specific example (default is cavity)');
fprintf('\n     1  Channel domain')
fprintf('\n     2  Flow over a backward facing step')
fprintf('\n     3  Lid driven cavity')
fprintf('\n     4  Colliding flow\n');
sn = default('',3);
if sn==1, 
system('/bin/cp ./stokes_flow/test_problems/poiseuille_flow.m ./stokes_flow/specific_flow.m');
system('/bin/cp ./stokes_flow/test_problems/poiseuille_bc.m ./stokes_flow/stream_bc.m');
   channel_stokes, xsolve_stokes  
elseif sn==2 
system('/bin/cp ./stokes_flow/test_problems/backwardstep_flow.m ./stokes_flow/specific_flow.m');
system('/bin/cp ./stokes_flow/test_problems/backwardstep_bc.m ./stokes_flow/stream_bc.m');
   longstep_stokes, solve_step_stokes  
elseif sn==3
   lid_model=default('cavity type leaky/tight/regularised 1/2/3 (regularised)',3); 
   if lid_model ==3    
system('/bin/cp ./stokes_flow/test_problems/regcavity_flow.m ./stokes_flow/specific_flow.m');
   elseif lid_model ==2    
system('/bin/cp ./stokes_flow/test_problems/tightcavity_flow.m ./stokes_flow/specific_flow.m');
   else
system('/bin/cp ./stokes_flow/test_problems/leakycavity_flow.m ./stokes_flow/specific_flow.m');
   end
system('/bin/cp ./stokes_flow/test_problems/zero_bc.m ./stokes_flow/stream_bc.m');
   square_stokes, solve_stokes  
elseif sn==4
system('/bin/cp ./stokes_flow/test_problems/collide_flow.m ./stokes_flow/specific_flow.m');
system('/bin/cp ./stokes_flow/test_problems/collide_bc.m ./stokes_flow/stream_bc.m');
   square_stokes, solve_stokes  
else
   error('reference problem datafile not found!')
end
