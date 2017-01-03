clear;
[fxpairs]=xlsread('C:/PairsWS/fx_portfolio');

lookback=5;
onewaytcost=1/10000; % 5 bps

%% mean reverting trading
totpnl=zeros(size(fxpairs,1), 1);
for p=1:2:3
    
    regression=ols(fxpairs(1:90, p), fxpairs(1:90, p+1));

    spread=fxpairs(:,p)-regression.beta*fxpairs(:,p+1);

    ma=smartMovingAvg(spread, lookback);
    mstd=smartMovingStd(spread, lookback);
    zscore=(spread-ma)./mstd;

    positions=[-zscore zscore];

    pnl=smartsum(backshift(1, positions).*([fxpairs(:, p) fxpairs(:, p+1)]-backshift(1, [fxpairs(:, p) fxpairs(:, p+1)]))./backshift(1, [fxpairs(:, p) fxpairs(:, p+1)])-onewaytcost*abs(positions-backshift(1, positions)), 2);
    totpnl=totpnl+pnl;
end

sharpe=sqrt(252)*smartmean(pnl)/smartstd(pnl) % annualized Sharpe ratio

plot(smartcumsum(pnl));

