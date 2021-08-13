% POISSON CONTROL:  J.W. Pearson, 30 July 2012
%
%   helpme_poissoncontrol - IFISS interactive help  for Poisson control
%
% Files | Poisson control problem and direct solution
%   ell_poissoncontrol - forms and solves matrix system on L-shaped domain
%   nonzerobc_input - imposes specific Dirichlet boundary conditions
%   poissoncontrol_errortest - computes error estimates for Problem 3 sol'n
%   poissoncontrol_rhs_custom - inputs specific desired state and BCs
%   poissoncontrol_rhs_ex1 - inputs desired state and BCs for Problem 1
%   poissoncontrol_rhs_ex2 - inputs desired state and BCs for Problem 2
%   poissoncontrol_rhs_ex3 - inputs desired state and BCs for Problem 3
%   poissoncontrol_rhs_ex4 - inputs desired state and BCs for Problem 4
%   square_poissoncontrol - forms and solves matrix system on square domain
%
% Files | iterative solution of matrix system
%   it_solve_poissoncontrol - solves iteratively the matrix system
%   it_solve_poissoncontrol_reduced - solves (reduced) 2x2 block system
%   mass_chebyshev - applies (1,1)-block preconditioner (Chebyshev)
%   mass_chebyshev_reduced - as above for (reduced) 2x2 block system
%   mass_diagonal - applies (1,1)-block preconditioner (diagonal approx.)
%   mass_diagonal_reduced - as above for (reduced) 2x2 block system
%   mass_exact - applies (1,1)-block preconditioner (exact solution)
%   mass_exact_reduced - as above for (reduced) 2x2 block system
%   mass_identity - identity operator for (1,1)-block
%   mg_poissoncontrol - sets up inputs for geometric multigrid solver
%   poissoncontrol_minres - MINRES algorithm for Poisson control problem
%   poissoncontrol_minres_reduced - MINRES for (reduced) 2x2 block system
%   schur_amg - applies Schur complement preconditioner (AMG)
%   schur_diagonal - applies Schur comp. preconditioner (diagonal approx.)
%   schur_exact - applies Schur complement preconditioner (exact solution)
%   schur_gmg - applies Schur complement preconditioner (GMG)
%   schur_identity - identity operator for Schur complement

