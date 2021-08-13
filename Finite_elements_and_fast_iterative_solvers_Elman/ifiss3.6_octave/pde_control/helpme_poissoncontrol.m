%HELPME_POISSONCONTROL IFISS interactive help facility for Poisson control
%   IFISS scriptfile: JWP; DJS; 30 July 2012.
% Copyright (c) 2012 D.J. Silvester, H.C. Elman, A. Ramage, J.W. Pearson

fprintf(' \n');
fprintf(' To solve a Poisson control problem, run a scriptfile:\n');
fprintf(' square_poissoncontrol or ell_poissoncontrol\n');
fprintf(' for a square or L-shaped domain respectively.\n');
%fprintf(' This will form the relevant matrix system, solve it directly,\n');
%fprintf(' and plot the solution. \n');
fprintf(' Reference problems 1,2,3 are defined on the square domain, and\n');
fprintf(' problem 4 is on the L-shaped domain.\n\n');

fprintf(' The user can choose the problem to be solved, the grid\n');
fprintf(' parameter (i.e. the mesh-size), the regularization parameter,\n');
fprintf(' whether the grid is uniform or stretched, the type of square or\n');
fprintf(' L-shape being considered, whether Q1 or Q2 basis functions are\n');
fprintf(' used, and whether or not to condense the system matrix size.\n\n');

fprintf(' To define the desired state and Dirichlet boundary conditions\n');
fprintf(' for a particular problem on a square domain, type \n');
fprintf(' help poissoncontrol_rhs_custom\n\n');

fprintf(' To solve a problem iteratively, run either \n');
fprintf(' it_solve_poissoncontrol or it_solve_poissoncontrol_reduced \n');
fprintf(' to solve the original, or the condensed system, respectively.\n\n');
%fprintf(' This functions, poissoncontrol_minres or\n');
%fprintf(' poissoncontrol_minres_reduced\n\n\n');
fprintf(' The user can choose the tolerance, the maximum number \n');
fprintf(' of iterations, and the preconditioning strategy to be used,\n');
fprintf(' potentially including options for Chebyshev semi-iteration,\n');
fprintf(' geometric multigrid or algebraic multigrid.\n\n');
