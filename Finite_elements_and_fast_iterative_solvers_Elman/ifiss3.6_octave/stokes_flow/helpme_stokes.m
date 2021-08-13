%HELPME_STOKES Stokes flow problem interactive help 
%   IFISS scriptfile: DJS; 20 April 2012. 
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 

fprintf(' \n');
fprintf(' To generate boundary and source term datafiles for the Stokes \n');
fprintf(' flow test problems, simply run the driver: stokes_testproblem\n');
fprintf(' \n');
fprintf(' Nonzero boundary conditions are set in the function\n');
fprintf(' /stokes_flow/specific_flow.m\n');
fprintf(' Streamfunction boundary conditions are set in the function\n');
fprintf(' /stokes_flow/stream_bc.m\n');
fprintf(' \n');
fprintf(' After a discrete problem is set up, inf-sup eigenvalues may\n');
fprintf(' be computed by calling the function infsup.m\n');
fprintf(' \n');
