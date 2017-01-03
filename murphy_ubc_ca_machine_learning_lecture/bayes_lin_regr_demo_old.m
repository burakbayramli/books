function bayes_lin_regr_demo_old()
% Demonstrate bayesian linear regression
% Based on code written by Frank Hutter

seed = 0;
randn('state', seed);
rand('state', seed);
useRBF = 1;
doSave = 0;
longRange = 0;
trainStep = 0.1;
%trainStep = 0.5;

if longRange
  xTrainRaw = [-9:trainStep:-8, 3:trainStep:3.5]'+0;
  xTestRaw = [-10:0.1:10]'+0;
else
  xTrainRaw = [-1:trainStep:-0.5,3:trainStep:3.5]'+0;
  xTestRaw = [-2:0.1:4.5]'+0;
end

yTrain = feval(@fun, xTrainRaw) + randn(size(xTrainRaw,1),1)*10;
if useRBF
  lo = min( min(xTrainRaw), min(xTestRaw) );
  hi = min( max(xTrainRaw), max(xTestRaw) );
  RBFcenters = linspace(lo, hi, 5)'
  %RBFcenters = linspace(-5, 5, 5)';
  xTrain = rbfBasis(xTrainRaw, RBFcenters);
else
  xTrain = polyBasis(xTrainRaw, 2);
end

yTestOpt = feval(@fun, xTestRaw);
if useRBF
  xTest = rbfBasis(xTestRaw, RBFcenters);
else
  xTest = polyBasis(xTestRaw, 2);
end

[N,M] = size(xTrain);
beta = 0.02; % observation precision 1/sigma_obs^2.
mean_0 = 0*ones(M,1); % Mx1 matrix, zero mean.
Sigma_0 = 1e4 * eye(M,M); % MxM matrix, wide prior.

[mu_w, Sigma_w] = bayesian_update(mean_0, Sigma_0, beta, xTrain, yTrain);
mu_w

[yPred, yPred_var] = bayes_fwd(mu_w, Sigma_w, beta, xTest);
[yPredTrain, yPredTrain_var] = bayes_fwd(mu_w, Sigma_w, beta, xTrain);

% Plot predictive distribution
figure(1); clf
hold off
subplot(111)
errorbar(xTestRaw(:,1), yPred, sqrt(yPred_var), 'kx-');
hold on
plot(xTestRaw(:,1),yTestOpt,'bx-');
h = errorbar(xTrainRaw(:,1), yPredTrain, sqrt(yPredTrain_var), 'gx');
h = plot(xTrainRaw(:,1), yTrain, 'ro');
set(h, 'linewidth', 3)
grid on

if doSave
fname = sprintf('bayes_lin_regr_demo_frank_rbf%d.eps', useRBF);
folder = 'C:\kmurphy\Teaching\stat406-spring06\Book\figures';
print(gcf, '-depsc', fullfile(folder, fname));

fname = sprintf('bayes_lin_regr_demo_frank_rbf%d.jpg', useRBF);
folder = 'C:\kmurphy\Teaching\stat406-spring06\Book\figures';
print(gcf, '-djpeg', fullfile(folder, fname));
end


% Plot samples from the posterior
figure(2); clf
plot(xTestRaw(:,1),yTestOpt,'bx-'); hold on
nsamples = 10;
for s=1:nsamples
  w = mvnrnd(mu_w, Sigma_w, 1);
  Y_mean = xTest * w(:);
  plot(xTestRaw(:,1), Y_mean, 'kx-');
end


if doSave
fname = sprintf('bayes_lin_regr_demo_frank_samples_rbf%d.eps', useRBF);
folder = 'C:\kmurphy\Teaching\stat406-spring06\Book\figures';
print(gcf, '-depsc', fullfile(folder, fname));

fname = sprintf('bayes_lin_regr_demo_frank_samples_rbf%d.jpg', useRBF);
folder = 'C:\kmurphy\Teaching\stat406-spring06\Book\figures';
print(gcf, '-djpeg', fullfile(folder, fname));
end



%%%%%%%%

function f = fun(x) %target function
f = x.^4;


%%%%%%%%
function [mu_w, Sigma_w] = bayesian_update(mu_w, Sigma_w, beta, X, Y)
Sigma_old_inv = inv(Sigma_w);
Sigma_w = pinv(Sigma_old_inv + (beta * X' * X));
mu_w = (Sigma_w*Sigma_old_inv) * mu_w + (beta * Sigma_w * X' * Y);


%%%%%%%%
function [Y_mean, Y_var] = bayes_fwd(mu_w, Sigma_w, beta, X)
Y_mean = X * mu_w;
for i=1:size(X,1)
    Y_var(i,1) = 1/beta + X(i,:) * Sigma_w * X(i,:)';
end
