%port_trade_truncate.m
clear;

load('SPX_20071123');

cl(end, :)=[];

ret=(cl-backshift(1, cl))./backshift(1, cl); % daily returns
retahead=(fwdshift(1, cl)-cl)./cl; 
marketRet=smartmean(retahead, 2); % equal weighted market index return

weights=-(ret-repmat(marketRet, [1 size(ret, 2)])); % weight of a stock is proportional to the negative distance to the market index.

load('C:/BacktestWS/weights_truncate');

assert(all(all(weights==weights_truncate)));
