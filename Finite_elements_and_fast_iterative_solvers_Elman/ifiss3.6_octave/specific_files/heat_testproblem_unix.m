%HEAT_TESTPROBLEM_UNIX sets up Examples H1 to H3
%   IFISS scriptfile: DJS; 9 May 2012.  
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
gohome
clear variables
fprintf('\nSpecification of reference heat equation problem.')
fprintf('\nchoose specific example');
fprintf('\n     1  Rectangle domain, differently heated sides')
fprintf('\n     2  L-shaped domain, line and point sources')
fprintf('\n     3  Backward-facing step domain, differently heated sides\n')
sn = default('',2);
%
if sn==1
system('/bin/cp ./diffusion/test_problems/zero_rhs.m ./diffusion/specific_rhs.m');
system('/bin/cp ./timestepping/test_problems/heat1_bc.m ./diffusion/specific_bc.m');
   box_heat
elseif sn==2
system('/bin/cp ./diffusion/test_problems/zero_rhs.m ./diffusion/specific_rhs.m');
system('/bin/cp ./timestepping/test_problems/heat2_bc.m ./diffusion/specific_bc.m');
   ell_heat
elseif sn==3
system('/bin/cp ./diffusion/test_problems/zero_rhs.m ./diffusion/specific_rhs.m');
system('/bin/cp ./timestepping/test_problems/heat3_bc.m ./diffusion/specific_bc.m');
   step_heat
else
   error('reference problem datafile not found!')
end
