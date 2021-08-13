%HELPME_CD convection-diffusion problem interactive help
%   IFISS scriptfile: DJS; 5 March 2005. 
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 

fprintf(' \n');
fprintf(' To generate convection diffusion matrices over a \n');
fprintf(' square shaped domain run the script file square_cd  or ref_cd\n');
fprintf(' Typing <CR> when prompted for input automatically gives the \n');
fprintf(' default choice. \n');
fprintf(' The convective wind is set in the user-definable function\n');
fprintf(' /convection/specific_wind.m\n');
fprintf(' \n');
fprintf(' To solve a convection-diffusion problem with a specified\n');
fprintf(' viscosity parameter run the script file: solve_cd\n');
fprintf(' Nonzero boundary conditions are set in the function\n');
fprintf(' /diffusion/specific_bc.m\n');
fprintf(' \n');
fprintf(' To generate boundary and source term datafiles for the diffusion \n');
fprintf(' test problems, simply run the driver: cd_testproblem\n');
fprintf(' \n');
