%HELPME_NAVIER Navier-Stokes flow problem interactive help 
%   IFISS scriptfile: DJS; 2 October 2013.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 

fprintf(' \n');
fprintf(' To generate boundary and source term datafiles for the Navier-Stokes\n'); 
fprintf(' flow test problems, simply run the driver: navier_testproblem\n');
fprintf(' Newton and Picard linearization is used with a direct linear solver\n');
fprintf(' \n');
fprintf(' Nonzero boundary conditions are set in the function\n');
fprintf(' /stokes_flow/specific_flow.m\n');
fprintf(' Streamfunction boundary conditions are set in the function\n');
fprintf(' /stokes_flow/stream_bc.m\n');
fprintf(' \n');
fprintf(' To test fast iterative solvers for the Navier-Stokes\n');
fprintf(' flow test problems, run the alternative driver: it_navier_testproblem\n');
fprintf(' \n');
