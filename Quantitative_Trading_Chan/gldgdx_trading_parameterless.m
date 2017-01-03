clear;

[gld]=xlsread('C:/PairsWS/GLD1');
[gdx]=xlsread('C:/PairsWS/GDX1');

plot(gld);
hold on;
plot(gdx, 'r');

results=cadf(gld, gdx, 0, 1)

regression=ols(gld(1:90), gdx(1:90))

spread=gld-regression.beta*gdx;

hold off;
plot(spread);

%% mean reverting trading

lookback=5;
ma=smartMovingAvg(spread, lookback);
mstd=smartMovingStd2(spread, lookback);
zscore=(spread-ma)./mstd;

positions=[-zscore zscore];

pnl=smartsum(backshift(1, positions).*([gld gdx]-backshift(1, [gld gdx]))./backshift(1, [gld gdx]), 2);
sharpe=sqrt(252)*smartmean2(pnl)/smartstd2(pnl) % annualized Sharpe ratio

plot(smartcumsum(pnl));

%% incorporating transaction costs
onewaytcost=5/10000; % 5 bps
pnl=smartsum(backshift(1, positions).*([gld gdx]-backshift(1, [gld gdx]))./backshift(1, [gld gdx])-onewaytcost*abs(positions-backshift(1, positions)), 2);
sharpeWithCost=sqrt(252)*smartmean2(pnl)/smartstd2(pnl) % annualized Sharpe ratio