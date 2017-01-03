function rlag=calculateReturns(prices, lag)
% rlag=calculateReturns(prices, lag) returns the lagged returns based on
% the price series

% rlag=log(prices)-backshift(lag, log(prices));
prevPrices=backshift(lag, prices);
rlag=(prices-prevPrices)./prevPrices;


