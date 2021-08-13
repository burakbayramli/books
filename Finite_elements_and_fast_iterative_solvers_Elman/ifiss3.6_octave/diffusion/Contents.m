% DIFFUSION
%
% Files
%   deriv        - evaluates derivatives of bilinear shape functions 
%   diffpost_bc  - postprocesses local Poisson error estimator 
%   diffpost_p   - legacy code | use diffpost_bc instead
%   diffpost_res - computes Q1 element residual error estimator 
%   ell_diff     - solve Poisson problem in L-shaped domain 
%   femq1_diff   - vectorized bilinear coefficient matrix generator
%   femq2_diff   - vectorized biquadratic coefficient matrix generator
%   gauss_source - evaluates source term at Gauss point 
%   helpme_diff  - diffusion problem interactive help 
%   lderiv       - evaluates derivatives of linear shape functions 
%   localbc_p    - imposes Dirichlet BC for Poisson error estimator 
%   lshape       - evaluates linear shape functions 
%   nonzerobc    - imposes Dirichlet boundary condition 
%   q1fluxjmps   - vectorised flux jumps for rectangular Q1 grid 
%   q1res_diff   - computes interior residuals for rectangular Q1 grid
%   qderiv       - evaluates derivatives of biquadratic shape functions 
%   qshape       - evaluates biquadratic shape functions 
%   quad_diff    - solve Poisson problem in quadrilateral domain 
%   shape        - evaluates bilinear shape functions 
%   specific_bc  - (current) problem boundary condition 
%   specific_rhs - (current) problem forcing function
%   square_diff  - solve Poisson problem in unit square domain 
%   cderiv              - evaluates derivatives of bicubic shape functions 
%   cshape              - evaluates bicubic shape functions 
%   diffpost_q1         - local Poisson error estimator for Q1 solution 
%   diffpost            - driver for a posteriori error postprocessing
%   diffpost_q1bc       - test code| alternative to diffpost_bc 
%   diffpost_q2_with_q4 - local Poisson error estimator for Q2 solution 
%   element_lusolve     - vectorized local backward-forward solves      
%   gausspoints_oned    - constructs one-dimensional Gauss Point Rule
%   gausspoints_twod    - constructs tensor product Gauss Point Rule
%   oldq1fluxjmps       - legacy code | replaced by q1fluxjmps 
%   q2fluxjmps          - vectorised flux jumps for rectangular Q2 grid 
%   qderiv_2            - second derivatives of biquadratic shape functions 
%   qqderiv             - derivatives of biquartic shape functions 
%   qqshape             - evaluates biquartic shape functions 
%   qshape_2            - evaluates biquadratic second derivatives 
