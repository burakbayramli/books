clear;

[gld]=xlsread('C:/PairsWS/GLD1');
[gdx]=xlsread('C:/PairsWS/GDX1');

plot(gld);
hold on; % keeping first series on plot
plot(gdx, 'r'); % plot second series in red
hold off; % don’t draw next plot on top

results=cadf(gld, gdx, 0, 1)

results2=cadf(gdx, gld, 0, 1)

results_johansen=johansen([gld gdx], 0, 1);

prt(results_johansen, strvcat({'GLD', 'GDX'}));

results_johansen2=johansen([gdx gld], 0, 1);

prt(results_johansen2, strvcat({'GDX'; 'GLD'}));

% use scatter plot, with gld on x-axis
scatter(gld, gdx);

regression=ols(gld(1:90), gdx(1:90))

spread=gld-regression.beta*gdx; % regression.beta=1.6294

% Compare with the hedgeRatio formed by the maximal eigenvector of johansen
% test:

% results_johansen_trainset=johansen([gld(1:90) gdx(1:90)], 0, 1);
% hedgeRatio=-results_johansen_trainset.evec(2,1)/results_johansen_trainset.evec(1,1) % hedgeRatio=1.3662

hold off;
plot(spread);

%% mean reverting trading

lookback=5;
ma=smartMovingAvg(spread, lookback);
mstd=smartMovingStd2(spread, lookback);
zscore=(spread-ma)./mstd;

entryZscore=0.5;
exitZscore=0;

longEntry=zscore < -entryZscore;
shortEntry=zscore > entryZscore;

longExit=zscore >= -exitZscore;
shortExit=zscore <= exitZscore;

positionsLong=NaN(size(spread)); % long spread positions
positionsLong(longEntry)=1;
positionsLong(longExit)=0;
positionsLong=fillMissingData(positionsLong);
positionsLong(isnan(positionsLong))=0;

positionsShort=NaN(size(spread)); % short spread positions
positionsShort(shortEntry)=-1;
positionsShort(shortExit)=0;
positionsShort=fillMissingData(positionsShort);
positionsShort(isnan(positionsShort))=0;

positionsSpread=positionsLong + positionsShort; % spread positions
positions=zeros(size(positionsSpread, 1), 2); % stocks positions
positions(positionsSpread>0, 1)=1;
positions(positionsSpread>0, 2)=-1;
positions(positionsSpread<0, 1)=-1;
positions(positionsSpread<0, 2)=1;

% Note that we need to sum across returns of gld and gdx
pnl=smartsum2(backshift(1, positions).*([gld gdx]-backshift(1, [gld gdx]))./backshift(1, [gld gdx]), 2);

plot(smartcumsum(pnl));

%% half-life computation
dz=fwdshift(1, spread)-spread;
dz(end)=[]; % remove the last NaN value
spread(end)=[]; % remove first value to match dz
regression=ols(dz, spread-mean(spread))
halflife=-log(2)/regression.beta
