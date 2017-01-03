% vector autoregressive function library -- Jim LeSage
%
%   
%             becm : performs Bayesian error correction model estimation
%           becm_d : demonstrate the use of becm
%           becm_g : Gibbs sampling estimates for Bayesian error correction 
%          becm_gd : An example of using becm_g(), 
%            becmf : estimates a Bayesian error correction model of order n
%          becmf_d : demonstrate the use of becmf
%          becmf_g : Gibbs sampling forecasts for Bayesian error 
%         becmf_gd : An example of using becmf_g(), 
%             bvar : Performs a Bayesian vector autoregression of order n
%           bvar_d : An example of using bvar(), 
%           bvar_g : Gibbs sampling estimates for Bayesian vector 
%          bvar_gd : An example of using bvar_g(), 
%            bvarf : Estimates a Bayesian vector autoregression of order n
%          bvarf_d : An example of using bvarf(), 
%          bvarf_g : Gibbs sampling forecasts for Bayesian vector 
%         bvarf_gd : An example of using bvarf_g(), 
%              ecm : performs error correction model estimation
%            ecm_d : demonstrate the use of ecm()
%             ecmf : estimates an error correction model of order n
%           ecmf_d : demonstrate the use of ecmf
%              irf : Calculates Impulse Response Function for VAR
%            irf_d : An example of using irf
%           irf_d2 : An example of using irf
%          lrratio : performs likelihood ratio test for var model
%        lrratio_d : demonstrate the use of lrratio()
%        make_html : makes HTML verion of contents.m files for the Econometrics Toolbox
%           pftest : prints VAR model ftests
%         pftest_d : An example of using pftest
%         pgranger : prints VAR model Granger-causality results
%          plt_var : plots VAR model actual vs predicted and residuals
%         plt_varg : Plots Gibbs sampled VAR model results
%          prt_var : Prints vector autoregressive models output
%         prt_varg : Prints vector autoregression output
%             recm : performs Bayesian error correction model estimation
%           recm_d : demonstrate the use of recm
%           recm_g : Gibbs sampling estimates for Bayesian error correction 
%          recm_gd : An example of using recm_g function
%            recmf : Estimates a Bayesian error correction model of order n
%          recmf_d : An example of using recmf(), 
%          recmf_g : Gibbs sampling forecasts for Bayesian error correction 
%         recmf_gd : An example of using recmf_g function
%             rvar : Estimates a Bayesian vector autoregressive model 
%           rvar_d : An example of using rvar() function
%           rvar_g : Gibbs estimates for a Bayesian vector autoregressive 
%          rvar_gd : An example of using rvar_g function
%            rvarb : Estimates a Bayesian vector autoregressive model 
%            rvarf : Estimates a Bayesian autoregressive model of order n
%          rvarf_d : An example of using rvarf(), 
%          rvarf_g : Gibbs forecasts for a Bayesian vector autoregressive 
%         rvarf_gd : An example of using rvarf_g(), 
%             svar : svar verifies the identification conditions for a given structural form
%           svar_d : An example of using svar                                               
%              var : performs vector autogressive estimation
%            var_d : An example of using var, pgranger, prt_var,plt_var
%             varf : estimates a vector autoregression of order n
%           varf_d : An example of using varf(),                               
