clear;

% 1 minute data on EWA-EWC
load('inputData_ETF', 'tday', 'syms', 'cl');
idxG=find(strcmp('GLD', syms));
idxU=find(strcmp('USO', syms));

x=cl(:, idxG);
y=cl(:, idxU);

% lookback period for calculating the dynamically changing hedge ratio
lookback=20; % Lookback set arbitrarily short
hedgeRatio=NaN(size(x, 1), 1);
for t=lookback:size(hedgeRatio, 1)
    regression_result=ols(log(y(t-lookback+1:t)), [log(x(t-lookback+1:t)) ones(lookback, 1)]);
    hedgeRatio(t)=regression_result.beta(1);
end

y2=[x y];

yport=sum([-hedgeRatio ones(size(hedgeRatio))].*log(y2), 2); % The net market value of the portfolio is same as the "spread"
hedgeRatio(1:lookback)=[]; % Removed because hedge ratio is indterminate
yport(1:lookback)=[]; 
y2(1:lookback, :)=[];
plot(yport);

% 
numUnits=-(yport-movingAvg(yport, lookback))./movingStd(yport, lookback); % units invested in portfolio.  movingAvg and movingStd are functions from epchan.com/book2
positions=repmat(numUnits, [1 size(y2, 2)]).*[-hedgeRatio ones(size(hedgeRatio))]; % [hedgeRatio -ones(size(hedgeRatio))] is the dollar capital allocation, while positions is the dollar capital in each ETF.
pnl=sum(lag(positions, 1).*(y2-lag(y2, 1))./lag(y2, 1), 2); % daily P&L of the strategy
ret=pnl./sum(abs(lag(positions, 1)), 2); % return is P&L divided by gross market value of portfolio
ret(isnan(ret))=0;

figure;
plot(cumprod(1+ret)-1); % Cumulative compounded return

fprintf(1, 'APR=%f Sharpe=%f\n', prod(1+ret).^(252/length(ret))-1, sqrt(252)*mean(ret)/std(ret));
% APR=0.088863 Sharpe=0.504153
