% Bayes_MCMC_2Dmarginal_MRSL.m
% function [bin_2Dic, bin_2Djc, bin_2Dp] = ...
%    Bayes_MCMC_2Dmarginal_MRSL(...
%        MRSLData, det_S_ref, ...
%        i_plot_2D, j_plot_2D, val_lo, val_hi, ...
%        N_bins, theta_0, MCOPTS);
%
% This routine uses MCMC simulation on the
% posterior density from multi-response
% data to compute the 2-D marginal posterior
% density of a pair of parameters using
% a histogram kernel method.
%
% The inputs are:
% The composite data set is described by the data structure
% MRSLData, with the fields:
% .num_sets = the number of response data sets in the
%             composite set of data
% .P = the number of parameters to be fitted
% .M = a vector containing for each data set the number
%      of predictor variables
% .L = a vector containing for each data set the number
%      of response variables
% .N = a vector containing for each data set the number
%      of experiments
% .X_pred_j = for each data set j, the N x M matrix of
%      predictor values
% .Y_j = for each data set j, the N x L matrix of
%      measured response values
% .fun_yhat_j = for each data set j, the name of a routine
%      that predicts the response values,
%        Y_hat = feval(fun_yhat_j, theta, X_pred_j);
%
% det_S_ref: a vector of values of det(S) for some reference
%      value of theta. This is used to avoid problems with
%      numerical overflow of the posterior density.

% i_plot_2D = the number of the first parameter
%
% j_plot_2D = the number of the second parameter
%
% val_lo = the lower bounds of the histogram regions
%
% val_up = the upper bounds of the histogram regions
%
% N_bins = the number of bins in each histogram region
%
% theta_0 = the initial parameter vector for the MCMC
%          simulation
%
% MCOPTS = the data structure that is passed to
%      Bayes_MCMC_pred_MR that implements the Monte
%      Carlo simulation
%
% The outputs are:
% bin_2Dic = the center coordinates of each bin in the
%          i_plot histogram
% bin_2Djc = the center coordinates of each bin in the
%          j_plot histogram
% bin_2Dp = the marginal probability values for each bin,
%         where bin_p(m,n) contains the marginal density
%         at theta(i_plot) = bin_ic(m),
%         theta(j_plot_ = bin_jc(n).
%
% K. J. Beers. MIT ChE. 12/17/2004. ver 12/17/2004

function [bin_2Dic, bin_2Djc, bin_2Dp] = ...
    Bayes_MCMC_2Dmarginal_MRSL(...
        MRSLData, det_S_ref, ...
        i_plot_2D, j_plot_2D, val_lo, val_hi, ...
        N_bins, theta_0, MCOPTS);

% initialize the output structures
bin_2Dic = zeros(N_bins,1);  bin_2Djc = bin_2Dic;
bin_delta = (val_hi-val_lo)./N_bins;
for k=1:N_bins
    bin_2Dic(k) = val_lo(1) + (k-0.5)*bin_delta(1);
    bin_2Djc(k) = val_lo(2) + (k-0.5)*bin_delta(2);
end
bin_2Dp = zeros(N_bins,N_bins);

% set name of routine that returns the
% appropriate predictors for use in
% generating a 2-D marginal density
fun_g = 'calc_g_2Dmarginal_MR';
Param.val_lo = val_lo;
Param.val_hi = val_hi;
Param.i_plot_2D = i_plot_2D;
Param.j_plot_2D = j_plot_2D;
Param.N_bins = N_bins;
Param.bin_delta = bin_delta;

% Now, perform the Markov Chain Monte Carlo
% (MCMC) simulation
g_pred = Bayes_MCMC_pred_MRSL(...
    MRSLData, det_S_ref, ...
    fun_g, theta_0, MCOPTS, Param);

% From the results of the MCMC simulation, generate
% the 2-D marginal density.
area_bin_inv = 1/bin_delta(1)/bin_delta(2);
for ki = 1:N_bins
    for kj = 1:N_bins
        label = (ki-1)*N_bins + kj;
        bin_2Dp(ki,kj) = g_pred(label)*area_bin_inv;
    end
end

% make contour and surface plots of the 2-D marginal density
% contour plot
figure;
contour(bin_2Dic,bin_2Djc,bin_2Dp);
xlabel(['\theta(', int2str(i_plot_2D),')']);
ylabel(['\theta(', int2str(j_plot_2D), ')']);
title('2-D marginal density from MCMC simulation');
% filled contour plot
figure;
contourf(bin_2Dic,bin_2Djc,bin_2Dp);  colorbar;
xlabel(['\theta(', int2str(i_plot_2D),')']);
ylabel(['\theta(', int2str(j_plot_2D), ')']);
title('2-D marginal density from MCMC simulation');
% surface plot
figure;
surf(bin_2Dic,bin_2Djc,bin_2Dp);
xlabel(['\theta(', int2str(i_plot_2D),')']);
ylabel(['\theta(', int2str(j_plot_2D), ')']);
zlabel(['p(\theta(', int2str(i_plot_2D), ', ', ...
        int2str(j_plot_2D), '| y)']);
title('2-D marginal density from MCMC simulation');

return;
    