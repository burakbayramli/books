% bollinger_5min_parameterless.m
clear;

[num]=xlsread('C:/BacktestWS/ES_2005_2007_5_min');
cl=num(:, end);

lookback=12;

position = -(cl- smartMovingAvg(cl, lookback))./smartMovingStd2(cl, lookback);

ret5min=(backshift(1, position).*(cl-backshift(1, cl)))./(backshift(1, abs(position)).*backshift(1, cl));

plot(smartcumsum(ret5min));

annavgret=252*(6.5*12+3)*smartmean2(ret5min)*100
annavgvol=sqrt(252*(6.5*12+3))*smartstd2(ret5min)*100
sharpe=annavgret/annavgvol
[maxDD maxDDD]=calculateMaxDD(smartcumsum(ret5min))

fprintf(1, '===With transaction costs of 1 bps===\n');
tcost=1/10000;

ret5min=(backshift(1, position).*(cl-backshift(1, cl))./backshift(1, cl)-tcost*abs(position-backshift(1, position)))./backshift(1, abs(position));    

plot(smartcumsum(ret5min));

annavgret=252*(6.5*12+3)*smartmean(ret5min)*100
annavgvol=sqrt(252*(6.5*12+3))*smartstd2(ret5min)*100
sharpe=annavgret/annavgvol
[maxDD maxDDD]=calculateMaxDD(smartcumsum(ret5min))

avgNumHoldBars=calculateAvgHoldday(position)

