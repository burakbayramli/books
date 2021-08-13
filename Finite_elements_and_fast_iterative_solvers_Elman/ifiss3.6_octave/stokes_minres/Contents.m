% STOKES_MINRES
%
% Files
%   algpost_q2p1    - computes Q2-P1 error estimate inside iteration 
%   algpost_q2q1    - computes Q2-Q1 error estimate inside iteration 
%   est_errorq2p1   - computes energy error estimate for Q2-P1 solution 
%   est_errorq2q1   - computes energy error estimate for Q2-Q1 solution 
%   est_minres      - MINRES with discretization error estimation
%   give            - computes Givens rotation matrix
%   helpme_stopping - optimal iterative solvers interactive help 
%   hydro           - removes hydrostatic pressure from solution vector
%   itsolve_stokes  - specialized iterative solution of Stokes problem
%   minres_esterror - MINRES with error estimation:minimizes ||r_k||_inv(P) 
%   param_est       - determines adaptive stopping criteria for EST_MINRES
