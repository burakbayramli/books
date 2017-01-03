clear;

% [num1 txt1]=xlsread('C:/PairsWS/AUDUSD');
% [num2 txt2]=xlsread('C:/PairsWS/USDCAD');
% 
% audcad=num2./num1;

[audcad txt]=xlsread('C:/PairsWS/AUDCAD');

audcad(isnan(audcad))=[];

plot(audcad);
results=adf(audcad, 0, 2)

%% mean reverting trading

lookback=5;
ma=smartMovingAvg(audcad, lookback);
mstd=smartMovingStd2(audcad, lookback);
zscore=(audcad-ma)./mstd;

entryZscore=0.5;
exitZscore=0;

longEntry=zscore < -entryZscore;
shortEntry=zscore > entryZscore;
% what type of array are longEntry and shortEntry?

longExit=zscore >= -exitZscore;
shortExit=zscore <= exitZscore;

positionsLong=NaN(size(audcad));
positionsLong(longEntry)=1;
positionsLong(longExit)=0;
% fillMissingData will carry forward previous position if no entry or exit
positionsLong=fillMissingData(positionsLong);
% clean up some initial NaN values in positionsLong before any entry or exit
positionsLong(isnan(positionsLong))=0;

positionsLong=fillMissingData(positionsLong);

positionsShort=NaN(size(audcad));
positionsShort(shortEntry)=-1;
positionsShort(shortExit)=0;
positionsShort=fillMissingData(positionsShort);
positionsShort(isnan(positionsShort))=0;

positions=positionsLong + positionsShort;

% pnl on a unit capital is really just the daily returns 
pnl=backshift(1, positions).*(audcad-backshift(1, audcad))./backshift(1, audcad);
% smartcumsum is the uncompounded return on strategy 
plot(smartcumsum(pnl));

