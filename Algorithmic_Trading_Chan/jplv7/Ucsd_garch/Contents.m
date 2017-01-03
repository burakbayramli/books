% UCSD_GARCH Toolbox.
% Version 2.0       01-Jan-2002
%
% Help and Documentation
%   ucsd_garch_demo          - A demo of the garch toolbox
%
% Main Univariate Mean Functions
%   armaxfilter              - Univariate ARMAX estimation
%   mafilter                 - Univariate MA estimation
%   garchinmean              - Univariate Garch-In-Mean estimation
%
% Main Univariate GARCH Functions
%   garchpq_eviews          - Univariare GARCH estimation without lower bound constraints; uses a penalty funcion(similar to eviews)
%   skewt_garch             - Univariat GARCH estimation with skew-t residuals(Hansen)
%   tarch                   - Univariate TARCH and GJR estimation
%   garchpq                 - Univariate garch estimation with analytic derivatives
%   fattailed_garch         - Univariate GARCH estimation with normal, Students T and Generalized Error Distribution
%   multi_garch             - Univariate GARCH proceeedure to estimate a variety of GARCH specifications including AP GARCH
%   egarch                  - Exponential garch estimation with normal, Students T and Generalized Error Distribution
%
% Main Multivariate Functions
%   cc_mvgarch              - Estimates Bollerslev's Constant Correlation MV Garch
%   dcc_mvgarch             - Estimates Engle and Sheppard's Dynamic Correlation MV Garch
%   o_mvgarch               - Estimates Orthogonal or Factor MV Garch
%   scalar_bekk_mvgarch     - Estimates Engle and Kroner's Scalar Bekk MV Garch 
%   diagonal_bekk_mvgarch   - Estimates Engle and Kroner's Diagonal Bekk MV Garch
%   full_bekk_mvgarch       - Estimates Engle and Kroner's Bekk MV Garch
%   Idcc_mvgarch            - Estimates Engle and Sheppards Integrated DCC MV Garch
%   scalar_bekk_T_mvgarch   - Estimates Scalar Bekk MV Garch with Multivariate T disturbances
%   diagonal_bekk_T_mvgarch - Estimates Diagonal Bekk MV Garch with Multivariate T disturbances
%   full_bekk_T_mvgarch     - Estimates Full Bekk MV Garch with Multivariate T disturbances
%
% Univariate Mean and GARCH Simulation
%   armaxsimulate             - Simulate an ARMAX model
%   garchsimulate             - Sumilate Univariate GARCH series with normal innovations
%   fattailed_garchsimulate   - Simulate Univariate GARCH series with Normal, Students T, or GED innovations
%   garcheviewssimulate       - Simulate a GARCH process with (some)negative smoothing terms
%   garchinmeansimulate       - Simulate a garch in mean model
%   egarchsimulate            - Simulate an EGARCH model
%   multigarchSimulate        - Simulate one of 8 different forms of GARCH
%   dcc_univariate_simulate   - likelihood function called from dcc_univariate_simulate
%
% Multivaraite GARCH Simulation
%   scalar_bekk_simulate     - Simulate a scalar BEKK
%   diagonal_bekk_simulate   - Simulate a diagonal BEKK
%   full_bekk_simulate       - Simulate a full BEKK model
%   cc_mvgarch_simulate      - Simulates Bollerslev's Constant Correlation MV Garch
%   dcc_simulate             - Simulates Engle and Sheppard's Dynamic Correlation MV Garch
%
% Univariate Mean Likelihood functions
%   garchinmeanlikelihood    - Likelihood funtion for garch in mean estimation
%   maxfilter_likelihood     - Likelihood function for MA estimation
%   armaxfilter_likelihood.m - likelihood function called from armaxfilter
%
% Univariate GARCH Likelihood Functions
%   garcheviewslikelihood     - likelihood function called from garchpq_eviews
%   skewt_garchlikelihood     - likelihood function called from skewt_garch
%       skewtdis_LL           - Log likelihod of a skew T distribution(helper)
%   garchlikelihood           - likelihood function called from garchpq
%   fattailed_garchlikelihood - likelihood function called from fattailed_garch
%   multi_garchlikelihood     - likelihood function called from multi_garch
%   egarchlikelihood          - likelihood function called from egarch
%   tarchlikelihood
%   
%
% Multivariate GARCH Likelihood Functions
%   cc_mvgarch_full_likelihood          - likelihood function called from cc_mvgarch_full_likelihood
%   dcc_mvgarch_full_likelihood         - likelihood function called from dcc_mvgarch_full_likelihood(correct)
%   dcc_mvgarch_likelihood              - likelihood function called from dcc_mvgarch_likelihood(restricted)
%   diagonal_bekk_mvgarch_likelihood    - likelihood function called from diagonal_bekk_mvgarch_likelihood
%   full_bekk_mvgarch_likelihood        - likelihood function called from full_bekk_mvgarch_likelihood
%   scalar_bekk_mvgarch_likelihood      - likelihood function called from scalar_bekk_mvgarch_likelihood
%   Idcc_mvgarch_full_likelihood        - likelihood function called from IDCC_mvgarch_likelihood(correct)
%   Idcc_mvgarch_likelihood             - likelihood function called from IDCC_mvgarch_likelihood(used in estimation)
%   scalar_bekk_T_est_likelihood        - likelihood function called from scalar_T_bekk_mvgarch_likelihood(used in estimation)
%   diagonal_bekk_T_est_likelihood      - likelihood function called from diagonal_T_bekk_mvgarch_likelihood(used in estimation)
%   full_bekk_T_est_likelihood          - likelihood function called from full_T_bekk_mvgarch_likelihood(used in estimation)
%   scalar_bekk_T_likelihood            - likelihood function called from scalar_T_bekk_mvgarch_likelihood(correct)
%   diagonal_bekk_T_likelihood          - likelihood function called from diagonal_T_bekk_mvgarch_likelihood(correct)
%   full_bekk_T_likelihood              - likelihood function called from full_T_bekk_mvgarch_likelihood(correct)
%
% Diagnostics
%   dcc_mvgarch_test          - Engle and Sheppards test for dynamic correlation
%   lilliefors                - Lillifors test for normality
%   ljq2                      - Ljung-Box Q Test
%   lmtest1                   - Lagrange Multiplier Test for autocorrelation
%   lmtest2                   - Lagrange Multiplier Test for autocorrelation in the squarred residuals, an ARCH test
%   jarquebera                - Jarque-Bera test for normality
%   shapirowilks              - Shapiro-Wilks Test for normality
%   shapirofrancia            - Shapiro-Francia Test for normality
%   kolmogorov                - Kolmorogov-Shmirnov non-parametric test
%   berkowitz                 - The berkowitz transform of the KS test
%
% Kernel Smoothing Routines
%   cosinus              - Cosinus kernel
%   epanechnikov         - Epanechnikov kernel
%   kern_dens_contour    - Bivariate kernel density plot of a density contour
%   kern_dens_plot       - Univariate kernel density plot
%   kern_dens_plot2      - 3d bivariate kernel density plot
%   normal               - Normal kernel
%   quartic              - Quaritc kernel
%   triangular           - Triangular kernel
%   triweight            - Triweight kernel
%   uniform              - Uniform kernel
%
% Bootstrap Routines
%   block_bootstrap      - Block time series bootstrap
%   bsds                 - Bootstrap Data Snooper(White 2000, Hansen 2001) with upper, lower and consistent pvals
%   bsds_studentized     - Bootstrap Data Snooper, using studentized bootstraps(Hansen 2001)
%   cont_bootstrap       - Continuous Bootstrap for unit root data
%   stationary_bootstrap - Stationary Bootstrap(Politis and Romano(1994)) for time series
%
% Univariate Density Functions  
%   exppowcdf            - Exponential Power Cumulative Density Function
%   exppowrnd            - Exponential Power Random number generator
%   exppowpdf            - Exponential Power Random Probability Density Function
%   gedcdf               - Generalized Error Distribution Cumulative Density Function
%   gedinv               - Generalized Error Distribution Inverse CDF
%   gedpdf               - Generalized Error Distribution Probability Density Function
%   gedrnd               - Generalized Error Distribution Random Number Generator
%   skewtdis_cdf         - Skew-T Cumulative Density Function
%   skewtdis_inv         - Skew-T Inverse CDF
%   skewtdis_pdf         - Skew-T Probability Density Function
%   skewtdis_rnd         - Skew-T Random Number Generator
%   stdtdis_cdf          - Standardized T distribution(unit variance for all nu) Cumulative Density Function
%   stdtdis_pdf          - Standardized T distribution(unit variance for all nu) Probability Density Function
%   stdtdis_rnd          - Standardized T distribution(unit variance for all nu) Random Number Generator
%
% Helper Functions
%   kscritical                - Lookup table for KS critical values
%   cc_ivech                  - Specialized ivech for correlation matrices
%   fx.mat                    - a data set for foreign exchange return used by the demos
%   multi_garch_paramsetup    - helper function for multi_garch
%   multi_garch_constraints   - helper function for multi_garch
%   dcc_hessian               - A modified version of HESSIAN for use in with CC_MVGARCH and DCC_MVGARCH
%   ivech                     - Creates a square lower triangular matrix, inverse of vech
%   vech                      - Takes teh half-vec of a square matrix, inverse of ivech
%   lagmatrix                 - Returns a matrix of lags of a dependant variable
%   pca                       - Performs Principal Componet Analysis
%
% C-MEX functions(shoudl be compilable on using and C compiler, binaries for Win32 provided)
%       NOTE: WHILE .M FILESARE AVAILABLE FOR ALL OF THESE, YOU SHOULD COMPILE THESE OR USE THE PROVIDED BINARIES
%       BINARIES END IN .DLL, MATLAB FUNCTIONS END IN .M, AND SOURCE ENDS IN .C
%   armaxcore           - Core routine for ARMAX
%   egarchcore          - Core routine for EGARCH
%   garchcore           - Core routine for GARCH and FATTAILED_GARCH
%   garchgrad           - Core routine for GARCH derivative estimation
%   garchinmeancore     - Core routine for Garch in mean estimation
%   multigarchcore      - Core routine for MULTIGARCH
%   ivech               - C version of ivech
%   vech                - C version of vech
%   maxcore             - Core routing for MA estimation
%   recserarcore        - C core for recserar
%   tarchcore           - Core routine for TARCH estimation
%   multigarchcore      - Core routine for MULTIGARCH
%
% NOTE: This toolbox requires both matlab optimization toolbox and the excellent J.P.LeSage Library
%       available from www.spatial-econometrics.com
% 
% Copyright (c) 2002 Kevin Sheppard   All Rights Reserved.
