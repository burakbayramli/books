% SOLVERS
%
% Files
%   a_cdt               - matrix-vector product for scalar operator
%   a_nst               - matrix-vector product for saddle-point operator
%   amg_coarsen_plot    - function for visualisation of amg coarsening
%   amg_grids_setup     - performs algebraic multigrid setup phase
%   amg_smoother        - performs smoothing 
%   amg_smoother_params - generates structure for AMG smoother parameters
%   amg_smoother_setup  - generates smoother data for AMG
%   amg_v_cycle         - performs one AMG V-cycle
%   bicgstab_ell        - bicgstab(ell) for right-preconditioned iterates
%   bicgstab_ell_r      - bicgstab(ell) iteration right preconditioning 
%   bsolve              - solves Galerkin system using backslash
%   cg_test             - CG convergence demo 
%   givapp              - apply a sequence of Givens rotations
%   gmres_r             - GMRES iteration with right preconditioning 
%   helpme_it           - iterative solvers interactive help 
%   helpme_mg           - geometric multigrid interactive help
%   idrs_r              - Induced Dimension Reduction right preconditioning
%   it_solve            - iterative solution of predefined steady problem
%   m_amgzt             - AMG preconditioner for scalar operator
%   m_amgzz             - AMG preconditioner with multiple v-cycles
%   m_bfbt              - ideal LSC preconditioner (unscaled)
%   m_diagt             - action of diagonal preconditioning operator
%   m_fp                - ideal PCD preconditioner
%   m_fp_amgz           - AMG iterated PCD preconditioner
%   m_fp_mg             - GMG iterated PCD preconditioner
%   m_ilut              - incomplete LU preconditioner
%   m_masscheb          - mass matrix Chebyshev preconditioning operator
%   m_massdiag          - mass matrix diagonal preconditioning operator
%   m_mg                - GMG preconditioner for scalar problems
%   m_nonet             - "no preconditioning" operator
%   m_st_amgz           - AMG block preconditioner for Stokes equations
%   m_st_block          - block preconditioner for Stokes equations
%   m_sxbfbt            - ideal stabilized LSC preconditioner
%   m_sxbfbt_amgz       - AMG iterated stabilized LSC preconditioner
%   m_xbfbt             - ideal least squares commutator preconditioner
%   m_xbfbt_amgz        - AMG iterated LSC preconditioner
%   m_xfp               - modified ideal PCD preconditioner
%   m_xfp_amgz          - AMG iterated modified PCD preconditioner
%   mg_cd               - GMG preconditioner for scalar CD problem
%   mg_cd_setup         - GMG convection-diffusion problem on square domain
%   mg_diff             - GMG preconditioner for diffusion problem
%   mg_diff_q2q1grid    - Q2-Q1 element grid generator for GMG
%   mg_diff_setup       - GMG diffusion problem on square domain
%   mg_diff_setup_ell   - GMG diffusion problem on L-shaped domain
%   mg_ellblock         - prolongation for part of L-shaped or step domain
%   mg_iter             - performs one GMG iteration
%   mg_post             - postsmoothing for GMG
%   mg_pre              - presmoothing for GMG
%   mg_prolong          - GMG prolongation operator for square domain
%   mg_prolong_ell      - GMG prolongation operator for L-shaped domain
%   mg_q1cd             - convection-diffusion matrix generator for GMG
%   mg_q1cd_supg        - streamline diffusion matrix generator for GMG 
%   mg_q1diff           - bilinear diffusion matrix generator for GMG 
%   mg_q1grid           - bilinear element grid generator for GMG
%   mg_smooth           - smoothers for GMG on square domain
%   mg_smooth_ell       - smoothers for GMG on L-shaped domain
%   mg_solve            - driver for GMG solution of predefined problem 
%   mg_zerobc           - imposes zero boundary conditions
%   milu0               - modified incomplete factorization with no fill-in
%   resplot             - plot residuals computed by iterative solvers
%   a_bouss             - matrix-vector product for Boussinesq operator
%   m_bouss_xbfbt       - ideal least squares commutator preconditioner
%   m_bouss_xbfbt_amgz  - AMG iterated LSC preconditioner
%   m_bouss_xfp         - modified ideal PCD preconditioner
%   m_bouss_xfp_amgz    - AMG iterated modified PCD preconditioner
%   snapshot_solvebouss - solution of predefined Boussinesq flow problem
%   snapshot_solveflow  - solution of predefined unsteady problem
%   ilu0                - incomplete factorization with no fill in
%   m_bcscale           - diagonal scaling for boundary-adjusted LSC 
%   m_bouss_xbfbt_bc    - ideal least squares commutator preconditioner
%   m_sxbfbt_bc         - boundary adjusted ideal stabilized LSC 
%   m_sxbfbt_bc_amgz    - AMG iterated boundary-adjusted LSC preconditioner
%   m_tfp_amgcheb       - AMG iterated modified PCD preconditioner
%   m_tfp               - modified ideal PCD preconditioner
%   m_tfp_amgz          - AMG iterated modified PCD preconditioner
%   m_xbfbt_bc          - boundary adjusted ideal LSC preconditioner
%   m_xbfbt_bc_amgz     - AMG iterated boundary-adjusted LSC preconditioner