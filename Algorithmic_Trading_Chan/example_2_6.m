clear;

% 1 minute data on EWA-EWC
load('inputData_ETF', 'tday', 'syms', 'cl');
idxA=find(strcmp('EWA', syms));
idxC=find(strcmp('EWC', syms));
disp(idxA); disp(idxC);

x=cl(:, idxA);
y=cl(:, idxC);

regression_result=ols(y, [x ones(size(x))]);
hedgeRatio=regression_result.beta(1);
disp(hedgeRatio);

plot(y-hedgeRatio*x);

% Assume a non-zero offset but no drift, with lag=1.
results=cadf(y, x, 0, 1); % cadf is a function in the jplv7 (spatial-econometrics.com) package. We pick y to be the dependent variable again.

% Print out results
prt(results);
