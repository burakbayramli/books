clear;

% [gld]=xlsread('GLD2');
% [gdx]=xlsread('GDX2');
% [uso]=xlsread('USO2');

[gld]=xlsread('GLD4.csv');
[gdx]=xlsread('GDX4.csv');
[uso]=xlsread('USO4.csv');

results_johansen=johansen([gld gdx uso], 0, 1);

prt(results_johansen, strvcat({'GLD', 'GDX', 'USO'}));

results_johansen_trainset=johansen([gld(1:90) gdx(1:90) uso(1:90)], 0, 1);

if (0)
    spread=smartsum(repmat(results_johansen_trainset.evec(:, 1)',...
        [size(gld, 1) 1]).*[gld gdx uso], 2); % regression.beta=1.6294
end

spread=[gld gdx uso]*results_johansen_trainset.evec(:, 1); % Net market value

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
if (0)
    positions=zeros(size(positionsSpread, 1), 3); % stocks positions in dollars
    
    positions(positionsSpread>0, 1)=...
        gld(positionsSpread>0).*results_johansen_trainset.evec(1, 1);
    positions(positionsSpread>0, 2)=...
        gdx(positionsSpread>0).*results_johansen_trainset.evec(2, 1);
    positions(positionsSpread>0, 3)=...
        uso(positionsSpread>0).*results_johansen_trainset.evec(3, 1);
    
    positions(positionsSpread<0, 1)=-gld(positionsSpread<0).*results_johansen_trainset.evec(1, 1);
    positions(positionsSpread<0, 2)=-gdx(positionsSpread<0).*results_johansen_trainset.evec(2, 1);
    positions(positionsSpread<0, 3)=-uso(positionsSpread<0).*results_johansen_trainset.evec(3, 1);
end

positions=zeros(size(positionsSpread, 1), 3); % stocks positions in shares

positions(positionsSpread<0, :)=-repmat(results_johansen_trainset.evec(:, 1)', [length(find(positionsSpread<0)) 1]);
positions(positionsSpread>0, :)=repmat(results_johansen_trainset.evec(:, 1)', [length(find(positionsSpread>0)) 1]);

% Note that we need to sum across returns of gld and gdx
% pnl=smartsum(backshift(1, positions).*([gld gdx uso]-backshift(1, [gld gdx uso]))./backshift(1, [gld gdx uso]), 2);
pnl=smartsum(backshift(1, positions).*([gld gdx uso]-backshift(1, [gld gdx uso])), 2);
% ret=pnl./smartsum(abs(backshift(1, positions)), 2);
ret=pnl./smartsum(abs(backshift(1, positions.*[gld gdx uso])), 2);

plot(smartcumsum(ret));

%% half-life computation
dz=fwdshift(1, spread)-spread;
dz(end)=[]; % remove the last NaN value
spread(end)=[]; % remove first value to match dz
regression=ols(dz, spread-mean(spread))
halflife=-log(2)/regression.beta
