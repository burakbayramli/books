% Bayes_MCMC_2Dmarginal_SR.m
% function [bin_ic, bin_jc, bin_p] = ...
%    Bayes_MCMC_2Dmarginal_SR( X_pred, y, ...
%        fun_yhat, i_plot, j_plot, val_lo, val_hi, ...
%        N_bins, theta_0, sigma_0, MCOPTS);
%
% This routine uses MCMC simulation on the
% posterior density from single-response
% data to compute the 2-D marginal posterior
% density of a pair of parameters using
% a histogram kernel method.
%
% The inputs are:
% X_pred = a matrix whose rows contain the predictor
%          variables for each experiment
% y = the vector of measured response values
% fun_yhat = the name of the function that computes
%          the predicted responses from the model,
%     y_hat = fevan(fun_yhat, theta, X_pred);
% i_plot = the number of the first parameter
% j_plot = the number of the second parameter
% val_lo = the lower bounds of the histogram regions
% val_up = the upper bounds of the histogram regions
% N_bins = the number of bins in each histogram region
% theta_0 = the initial parameter vector for the MCMC
%          simulation
% sigma_0 = the initial sigma value for the MCMC
%          simulation
% MCOPTS = the data structure that is passed to
%      Bayes_MCMC_pred_SR that implements the Monte
%      Carlo simulation
%
% The outputs are:
% bin_ic = the center coordinates of each bin in the
%          i_plot histogram
% bin_jc = the center coordinates of each bin in the
%          j_plot histogram
% bin_p = the marginal probability values for each bin,
%         where bin_p(m,n) contains the marginal density
%         at theta(i_plot) = bin_ic(m),
%         theta(j_plot_ = bin_jc(n).
%
% K. J. Beers. MIT ChE. 12/13/2004. ver 12/13/2004

function [bin_ic, bin_jc, bin_p] = ...
    Bayes_MCMC_2Dmarginal_SR( X_pred, y, ...
        fun_yhat, i_plot, j_plot, val_lo, val_hi, ...
        N_bins, theta_0, sigma_0, MCOPTS);

% initialize the output structures
bin_ic = zeros(N_bins,1);  bin_jc = bin_ic;
bin_delta = (val_hi-val_lo)./N_bins;
for k=1:N_bins
    bin_ic(k) = val_lo(1) + (k-0.5)*bin_delta(1);
    bin_jc(k) = val_lo(2) + (k-0.5)*bin_delta(2);
end
bin_p = zeros(N_bins,N_bins);

% set name of routine that returns the
% appropriate predictors for use in
% generating a 2-D marginal density
fun_g = 'calc_g_2Dmarginal';
Param.val_lo = val_lo;
Param.val_hi = val_hi;
Param.i_plot = i_plot;
Param.j_plot = j_plot;
Param.N_bins = N_bins;
Param.bin_delta = bin_delta;

% Now, perform the Markov Chain Monte Carlo
% (MCMC) simulation
g_pred = Bayes_MCMC_pred_SR(X_pred, y, ...
    fun_yhat, fun_g, theta_0, sigma_0, MCOPTS, Param);

% From the results of the MCMC simulation, generate
% the 2-D marginal density.
area_bin_inv = 1/bin_delta(1)/bin_delta(2);
for ki = 1:N_bins
    for kj = 1:N_bins
        label = (ki-1)*N_bins + kj;
        bin_p(ki,kj) = g_pred(label)*area_bin_inv;
    end
end

% make contour and surface plots of the 2-D marginal density
% contour plot
figure;
contour(bin_ic,bin_jc,bin_p);
xlabel(['\theta(', int2str(i_plot),')']);
ylabel(['\theta(', int2str(j_plot), ')']);
title('2-D marginal density from MCMC simulation');
% filled contour plot
figure;
contourf(bin_ic,bin_jc,bin_p);  colorbar;
xlabel(['\theta(', int2str(i_plot),')']);
ylabel(['\theta(', int2str(j_plot), ')']);
title('2-D marginal density from MCMC simulation');
% surface plot
figure;
surf(bin_ic,bin_jc,bin_p);
xlabel(['\theta(', int2str(i_plot),')']);
ylabel(['\theta(', int2str(j_plot), ')']);
zlabel(['p(\theta(', int2str(i_plot), ', ', ...
        int2str(j_plot), '| y)']);
title('2-D marginal density from MCMC simulation');

return;
    