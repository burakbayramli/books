function bayes_lin_regr_demo()
% Demonstrate bayesian linear regression
% Based on code written by Frank Hutter

seed = 0;
randn('state', seed);
rand('state', seed);
useRBF = 0;
doSave = 0;
longRange = 0;
trainStep = 0.1;
%trainStep = 0.5;
doStandardize = 1

if doStandardize
  figNum=1;
else
  figNum=3;
end

if longRange
  xTrainRaw = [-9:trainStep:-8, 3:trainStep:3.5]'+0;
  xTestRaw = [-10:0.1:10]'+0;
else
  xTrainRaw = [-1:trainStep:-0.5,3:trainStep:3.5]'+0;
  xTestRaw = [-2:0.1:4.5]'+0;
end

yTrain = feval(@fun, xTrainRaw) + randn(size(xTrainRaw,1),1)*10;
yTestOpt = feval(@fun, xTestRaw);

if doStandardize
  % We must standardize before doing an RBF expansion.
  % For polynomial basis, we can standardize before or afterwords.
  [xTrain, muTrain, sigmaTrain] = standardizeCols(xTrainRaw);
  [xTest] = standardizeCols(xTestRaw, muTrain, sigmaTrain);
else
  xTrain = xTrainRaw;
  xTest = xTestRaw;
end

if useRBF
  lo = min( min(xTrain), min(xTest) );
  hi = min( max(xTrain), max(xTest) );
  RBFcenters = linspace(lo, hi, 5)'
  %RBFcenters = linspace(-5, 5, 5)';
  xTrain = rbfBasis(xTrain, RBFcenters);
  xTest = rbfBasis(xTest, RBFcenters);
else
  xTrain = polyBasis(xTrain, 2);
  xTest = polyBasis(xTest, 2);
end

%xTrain = [ones(size(xTrain,1), 1) xTrain];
%xTest = [ones(size(xTest,1), 1) xTest];


[N,p] = size(xTrain);
beta = 0.02; % observation precision 
alpha = 1e-4; % prior precision
% We do not want to regularize the first component of the feature vector
% so we set the precision to 0
prior_precision = alpha*eye(p);
prior_prior(1,1) = 0;

Sigma_w = pinv(prior_precision + beta*xTrain'*xTrain);
mu_w = beta * Sigma_w * xTrain' * yTrain % ridge regression


[yPred, yPred_var] = bayes_fwd(mu_w, Sigma_w, beta, xTest);
[yPredTrain, yPredTrain_var] = bayes_fwd(mu_w, Sigma_w, beta, xTrain);

% Plot predictive distribution
figure(figNum); clf
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
figure(figNum+1); clf
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
f = 10 + x + x.^4;
%f = x.^4;


%%%%%%%%
function [Y_mean, Y_var] = bayes_fwd(mu_w, Sigma_w, beta, X)
Y_mean = X * mu_w;
for i=1:size(X,1)
    Y_var(i,1) = 1/beta + X(i,:) * Sigma_w * X(i,:)';
end
