% spatial autoregressive model estimation functions
%
%       beta_prior : construct beta-prior for rho over -1,1 interval
%   compare_models : An example of model comparison using log marginal posterior
%  compare_weights : An example of model comparison using sar_c() function
% compare_weights2 : An example of model comparison using sar_g() function
%           f2_sar : evaluates log-likelihood -- given ML estimates
%            f_sar : evaluates concentrated log-likelihood for the 
%        make_html : makes HTML verion of contents.m files for the Econometrics Toolbox
%          plt_sar : Plots output using SAR model results structures
%          prt_sar : Prints output using SAR results structures
%              sar : computes spatial autoregressive model estimates
%            sar_c : Bayesian log-marginal posterior for the spatial autoregressive model
%            sar_d : An example of using sar() 
%           sar_d2 : An example of using sar_g() on a large data set   
%           sar_d3 : An example of using sar on a large data set   
%            sar_g : Bayesian estimates of the spatial autoregressive model
%           sar_gd : An example of using sar_g() Gibbs sampling
%          sar_gd2 : An example of using sar_g() on a large data set   
%          sar_gd3 : An example of using sar_g() on a large data set   
%          sar_gd4 : An example of using priors with sar_g() Gibbs sampling
%           sar_gv : Bayesian estimates of the spatial autoregressive model
%          sar_gvd : An example of using sar_gv() Gibbs sampling
%           sarp_g : Bayesian estimates of the spatial autoregressive probit model
%          sarp_gd : An example of using sarp_g() Gibbs sampling
%         sarp_gd2 : An example of using sarp_g() on a large data set   
%         sarp_gd3 : An example of using sarp_g()
%           sart_g : Bayesian estimates of the spatial autoregressive tobit model
%          sart_gd : An example of using sart_g() Gibbs sampling
%         sart_gd2 : An example of using sart_g() on a large data set   
