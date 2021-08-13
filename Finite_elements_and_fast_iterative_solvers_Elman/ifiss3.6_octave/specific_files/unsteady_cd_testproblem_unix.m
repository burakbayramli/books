%UNSTEADY_CD_TESTPROBLEM_UNIX sets up Examples T-CD1 to T-CD4
%   IFISS scriptfile: DJS; 9 May 2012. 
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
gohome
clear variables 
fprintf('\nspecification of reference unsteady convection-diffusion problem.')
fprintf('\nchoose specific example');
fprintf('\n     2  Vertical wind, characteristic layers')
fprintf('\n     4  Recirculating wind\n')
sn = default('',2);
%
if sn==4
system('/bin/cp ./convection/test_problems/circular_wind.m ./convection/specific_wind.m');
system('/bin/cp ./convection/test_problems/hotwall_bc.m ./diffusion/specific_bc.m');
square_cd, unsteady_cd
elseif sn==2
system('/bin/cp ./convection/test_problems/ref_wind.m ./convection/specific_wind.m');
system('/bin/cp ./convection/test_problems/ref_bc.m ./diffusion/specific_bc.m');
ref_cd, unsteady_cd
else
error('reference problem datafile not found!')
end
