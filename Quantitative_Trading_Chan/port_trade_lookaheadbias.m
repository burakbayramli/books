% port_trade_lookaheadbias.m
clear;

load('SPX_20071123');

ret=(cl-backshift(1, cl))./backshift(1, cl);
retahead=(fwdshift(1, cl)-cl)./cl; % daily returns

marketRet=smartmean(retahead, 2); % equal weighted market index return

weights=-(retahead-repmat(marketRet, [1 size(retahead, 2)])); % weight of a stock is proportional to the negative distance to the market index.

weights_truncate=weights(1:end-1, :);

save('C:/BacktestWS/weights_truncate', 'weights_truncate');
