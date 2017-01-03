clear;

% 1 minute data on USDCAD
load('inputData_USDCAD', 'tday', 'hhmm', 'cl');

% Select the daily close at 16:59 ET.
y=cl(hhmm==1659);

plot(y);

% Assume a non-zero offset but no drift, with lag=1.
results=adf(y, 0, 1); % adf is a function in the jplv7 (spatial-econometrics.com) package.

% Print out results
prt(results);

% Augmented DF test for unit root variable:                   variable   1 
%  ADF t-statistic       # of lags   AR(1) estimate 
%        -1.840744               1         0.994120 
% 
%    1% Crit Value    5% Crit Value   10% Crit Value 
%           -3.458           -2.871           -2.594 

% Find Hurst exponent

H=genhurst(log(y), 2);
fprintf(1, 'H2=%f\n', H);

% Variance ratio test from Matlab Econometrics Toolbox
[h,pValue]=vratiotest(log(y));


fprintf(1, 'h=%i\n', h); % h=1 means rejection of random walk hypothesis, 0 means it is a random walk.
fprintf(1, 'pValue=%f\n', pValue); % pValue is essentially the probability that the null hypothesis (random walk) is true.


% Output: 
% h=0
% pValue=0.367281

% Find value of lambda and thus the halflife of mean reversion by linear regression fit
ylag=lag(y, 1);  % lag is a function in the jplv7 (spatial-econometrics.com) package.
deltaY=y-ylag;
deltaY(1)=[]; % Regression functions cannot handle the NaN in the first bar of the time series.
ylag(1)=[];
regress_results=ols(deltaY, [ylag ones(size(ylag))]); % ols is a function in the jplv7 (spatial-econometrics.com) package.
halflife=-log(2)/regress_results.beta(1);

fprintf(1, 'halflife=%f days\n', halflife);

% halflife=115.209794 days

% Apply a simple linear mean reversion strategy to USDCAD
lookback=round(halflife); % setting lookback to the halflife found above
mktVal=-(y-movingAvg(y, lookback))./movingStd(y, lookback); % capital in number of shares invested in USDCAD. movingAvg and movingStd are functions from epchan.com/book2
pnl=lag(mktVal, 1).*(y-lag(y, 1))./lag(y, 1); % daily P&L of the strategy
pnl(isnan(pnl))=0;
figure;
plot(cumsum(pnl)); % Cumulative P&L