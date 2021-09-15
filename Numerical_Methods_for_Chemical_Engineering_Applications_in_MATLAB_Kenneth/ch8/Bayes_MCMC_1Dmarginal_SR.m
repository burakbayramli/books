% Bayes_MCMC_1Dmarginal_SR.m
% function [bin_c, bin_p, frac_above, frac_below] = ...
%    Bayes_MCMC_1Dmarginal_SR( X_pred, y, ...
%        fun_yhat, j_plot, val_lo, val_hi, ...
%        N_bins, theta_0, sigma_0, MCOPTS);
%
% This routine uses MCMC simulation on the
% posterior density from single-response
% data to compute the 1-D marginal posterior
% density of one or more parameters from a
% histogram-kernel method.
%
% The inputs are:
% X_pred = a matrix whose rows contain the predictor
%          variables for each experiment
% y = the vector of measured response values
% fun_yhat = the name of the function that computes
%          the predicted responses from the model,
%     y_hat = fevan(fun_yhat, theta, X_pred);
% j_plot = the numbers of the parameters whose 1-D
%          marginal posterior densities are desired
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
% bin_c = the centers of the bins, where
%         bin_c(m,k) contains the center site
%         of bin # k for parameter # j_plot(m)
% bin_p = the marginal probability values for each bin,
%         normalized to the fraction of the total
%         number of samples that lie within the
%         histogram region, bin_p(m,k) is the prob.
%         value in bin # k for parameter # j_plot(m)
% frac_above = the fractions of samples that lie above
%         the histogram regions
% frac_below = the fractions of samples that lie below
%         the histogram regions
%
% K. J. Beers. MIT ChE. 12/8/2004. ver 12/10/2004

function [bin_c, bin_p, frac_above, frac_below] = ...
    Bayes_MCMC_1Dmarginal_SR( X_pred, y, ...
        fun_yhat, j_plot, val_lo, val_hi, ...
        N_bins, theta_0, sigma_0, MCOPTS);

% extract the number of parameters to plot
N_plot = length(j_plot);
N_hist = N_bins + 2;
N_tot = N_hist*N_plot;

% initialize the output structures
bin_c = zeros(N_plot,N_bins);
bin_delta = (val_hi-val_lo)./N_bins;
for m=1:N_plot
    for k=1:N_bins
        bin_c(m,k) = val_lo(m) + (k-0.5)*bin_delta(m);
    end
end
bin_p = zeros(N_plot,N_bins);
frac_above = zeros(size(j_plot));
frac_below = zeros(size(j_plot));
frac_in_hist = zeros(size(j_plot));

% set name of routine that returns the
% appropriate predictors for use in
% generating a 1-D marginal density
fun_g = 'calc_g_1Dmarginal';
Param.val_lo = val_lo;
Param.val_hi = val_hi;
Param.j_plot = j_plot;
Param.N_bins = N_bins;
Param.bin_delta = bin_delta;

% Now, perform the Markov Chain Monte Carlo
% (MCMC) simulation
g_pred = Bayes_MCMC_pred_SR(X_pred, y, ...
    fun_yhat, fun_g, theta_0, sigma_0, MCOPTS, Param);

% Extract from the results the 1D_marginal probability
% distribution, and plot the results for each of
% the parameters whose marginal density has been estimated.
for m=1:N_plot
    offset = (m-1)*N_hist;
    frac_below(m) = g_pred(offset+N_bins+1);
    frac_above(m) = g_pred(offset+N_bins+2);
    frac_in_hist(m) = 1.0 - frac_above(m) - frac_below(m);

    % compute the probability in each bin
    bin_p(m,1:N_bins) = ...
        (g_pred(offset+1:offset+N_bins)')./bin_delta(m);

    % make plot of the computed marginalized
    % posterior density
    figure;
    plot(bin_c(m,:),bin_p(m,:));
    xlabel(['\theta(', int2str(j_plot(m)),')']);
    ylabel(['p( \theta(', int2str(j_plot(m)), ') | y )']);
    title(['1-D marginal density from MCMC, ', ...
        'total prob. = ', num2str(frac_in_hist(m))]);
end

return;
    