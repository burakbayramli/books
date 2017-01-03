clear;

% 1 minute data on EWA-EWC
load('inputData_ETF', 'tday', 'syms', 'cl');
idxG=find(strcmp('GLD', syms));
idxU=find(strcmp('USO', syms));

x=cl(:, idxG);
y=cl(:, idxU);

lookback=20; % Lookback is set arbitrarily
ratio=y./x;
ratio(1:lookback)=[]; % Removed to have same test set as price spread and log price spread strategies
x(1:lookback)=[];
y(1:lookback)=[];
plot(ratio);


% 
%  Apply a simple linear mean reversion strategy to GLD-USO
numUnits=-(ratio-movingAvg(ratio, lookback))./movingStd(ratio, lookback); % units invested in the portfolio. movingAvg and movingStd are functions from epchan.com/book2
positions=repmat(numUnits, [1 2]).*[-ones(size(x, 1), 1) ones(size(x, 1), 1)]; % positions in dollar invested
pnl=sum(lag(positions, 1).*([x y]-lag([x y], 1))./lag([x y], 1), 2); % daily P&L of the strategy
ret=pnl./sum(abs(lag(positions, 1)), 2);
ret(isnan(ret))=0;

figure;
plot(cumprod(1+ret)-1); % Cumulative compounded return

fprintf(1, 'APR=%f Sharpe=%f\n', prod(1+ret).^(252/length(ret))-1, sqrt(252)*mean(ret)/std(ret));

% APR=-0.141522 Sharpe=-0.746663
