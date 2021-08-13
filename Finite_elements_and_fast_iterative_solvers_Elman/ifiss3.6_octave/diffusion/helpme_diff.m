%HELPME_DIFF diffusion problem interactive help 
%   IFISS scriptfile: DJS; 4 March 2005. 
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 

fprintf(' \n');
fprintf(' To generate and solve Poisson''s equation over a\n');
fprintf(' square-shaped domain, run the script file: square_diff \n');
fprintf(' Typing <CR> when prompted for input automatically gives the\n'); 
fprintf(' default choice. For an L-shaped domain run the script file: ell_diff\n');
fprintf(' For a quadrilateral shaped domain run: quad_diff\n');
fprintf(' \n');
fprintf(' Nonzero boundary conditions and forcing terms are set\n');
fprintf(' in the user-defined functions: /diffusion/specific_rhs.m and\n'); 
fprintf(' /diffusion/specific_bc.m\n');
fprintf(' \n');
fprintf(' To generate boundary and source term datafiles for the diffusion\n');
fprintf(' test problems, simply run the driver: diff_testproblem\n');
fprintf(' \n');
