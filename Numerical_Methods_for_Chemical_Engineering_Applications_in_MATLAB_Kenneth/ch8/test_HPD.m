% test_HPD.m
% This routine tests the generation of HPD for a
% simple model where the HPD should agree with
% the t-distribution.
% K.J. Beers. MIT ChE. 8/24/2005

%  generate the data set
N = 5;  mu = 0;  sigma = 1;
y = mu*ones(N,1) + sigma*randn(N,1),  % response vector
X_pred = ones(N,1);

% get least-squares fit
y_mean = mean(y),
sample_var = var(y,1); % compute sample variance
sample_std = sqrt(sample_var),  % sample standard deviation

% use regress to find least-squares fit
[theta,theta_CI] = regress(y,X_pred,0.05);
theta, theta_CI,

% plot posterior from t-distribution
nu = N - 1;  % number of degrees of freedom
theta_plot = linspace(-4*sigma,4*sigma,200);
t_plot = (theta_plot - y_mean)/(sample_std/sqrt(N));
p_tdist_plot = tpdf(t_plot,nu);
% normalize to get posterior
var1 = trapz(theta_plot,p_tdist_plot);
p_tdist_plot = p_tdist_plot/var1;
% plot posterior density
fig_t = figure;
plot(theta_plot,p_tdist_plot);
xlabel('\theta');  ylabel('p(\theta | y)');
title('Posterior from t distribution');
% add to graph lines for CI boundaries
p_max = max(p_tdist_plot);
hold on;
line([theta_CI(1) theta_CI(1)], [0 p_max]);
line([theta_CI(2) theta_CI(2)], [0 p_max]);


% Now, generate marginal posterior from MCMC
MCOPTS.N_equil = 10000;
MCOPTS.N_samples = 100000;
[bin_1Dc, bin_1Dp, frac_above, frac_below] = ...
    Bayes_MCMC_1Dmarginal_SR( ...
        X_pred, y, 'calc_yhat_linear_model', ...
        1, -4*sigma, 4*sigma, 200, theta, sigma, MCOPTS);
% make figure of posterior distribution from MCMC
fig_MCMC = figure;
plot(bin_1Dc, bin_1Dp);
xlabel('\theta');  ylabel('p(\theta | y)');
title('Posterior from MCMC simulation');

% comptue HPD regions
[HPD_lo, HPD_hi] = Bayes_1D_HPD_SR(...
    bin_1Dc, bin_1Dp, 1, 0.05);
HPD_lo, HPD_hi,
% add lines to figure at bounds of HPD regions
figure(fig_MCMC);
p_max = max(bin_1Dp);
hold on;
line([HPD_lo HPD_lo], [0 p_max]);
line([HPD_hi HPD_hi], [0 p_max]);

% make another plot that shows both t-distribution
% posterior and the marginal posterior from MCMC
fig_compare = figure;
plot(theta_plot,p_tdist_plot, '-.');  hold on;
plot(bin_1Dc, bin_1Dp);
xlabel('\theta');  ylabel('p(\theta | y)');
legend('t dist.','MCMC');
title('Posterior distributions');



