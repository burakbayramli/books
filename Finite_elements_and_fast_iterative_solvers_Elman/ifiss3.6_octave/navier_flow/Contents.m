% NAVIER_FLOW
%
% Files
%   Cpre_q1p0             - generate stabilization matrices for Q1-P0 
%   Cpre_q1q1             - generate stabilization matrices for Q1-Q1 
%   fpsetup_q0            - Q0 pressure convection-diffusion matrix 
%   fpsetup_q1            - Q1 pressure convection-diffusion matrix 
%   fpsetup_q2p1          - Q2-P1 pressure convection-diffusion matrix 
%   fpszsetup_q1          - modified Q1 PCD matrix with stabilization 
%   fpzsetup_q0           - modified Q0 PCD matrix 
%   fpzsetup_q1           - modified Q1 PCD matrix 
%   fpzsetup_q2p1         - modified P1 PCD matrix 
%   helpme_navier         - Navier-Stokes flow problem interactive help 
%   localbc_xycd          - imposes vector BC for Poisson error estimator 
%   navier_q1             - Q1 convection matrix 
%   navier_q2             - Q2 convection matrix 
%   navierpost_q1p0_bc    - postprocesses Poisson error estimator 
%   navierpost_q1p0_p     - computes Poisson error estimator for Q1-P0 
%   newton_q1             - Q1 convection derivative matrices 
%   newton_q2             - Q2 convection derivative matrices 
%   newtonbc              - imposes Dirichlet BC on Jacobian
%   null_pressure_index   - index associated with the pressure nullspace
%   pressurebc            - fixes singularity in Laplacian matrix
%   solve_navier          - solve singular Navier-Stokes problem
%   solve_channel_navier  - solve Navier-Stokes problem in channel domain
%   solve_obstacle_navier - solve Navier-Stokes problem in obstacle domain
%   solve_plate_navier    - solve Navier-Stokes problem in slit domain
%   solve_step_navier     - solve Navier-Stokes problem in step domain
%   navierpost            - estimates Q2-P1 or Q2-Q1 NS error distribution 
%   navierpost_q2p1       - vectorized Q2-P1 error estimator 
%   navierpost_q2q1       - vectorized Q2-Q1 error estimator 
%   fpxsetup_q1           - modified Q1 PCD matrix (ESW2005 version)
%   it_nstokes            - iterative solver for flow linear system
%   it_solve_navier       - iterative solution of N-S in square domain
%   it_solve_step_navier  - iterative solution of N-S in step domain

