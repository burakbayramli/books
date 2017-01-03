% 
% written by:
% Ernest Chan
%
% Author of “Quantitative Trading: 
% How to Start Your Own Algorithmic Trading Business”
%
% ernest@epchan.com
% www.epchan.com

clear;

startDate=20060101;
endDate=20061231;

load('SPX_20071123', 'tday', 'stocks', 'cl');

dailyret=(cl-lag1(cl))./lag1(cl); % daily returns

marketDailyret=smartmean(dailyret, 2); % equal weighted market index return

weights=-(dailyret-repmat(marketDailyret, [1 size(dailyret, 2)]))./repmat(smartsum(isfinite(cl), 2), [1 size(dailyret, 2)]); % weight of a stock is proportional to the negative distance to the market index.

weights(~isfinite(cl) | ~isfinite(lag1(cl)))=0; % those stocks that do not have valid prices or daily returns are excluded.

dailypnl=smartsum(lag1(weights).*dailyret, 2);

dailypnl(tday < startDate | tday > endDate) = []; % remove pnl outside of our dates of interest

sharpe=sqrt(252)*smartmean(dailypnl, 1)/smartstd(dailypnl, 1) % Sharpe ratio should be about 0.25

% daily pnl with transaction costs deducted
onewaytcost=0.0005; % assume 5 basis points

weights(tday < startDate | tday > endDate, :) = []; % remove weights outside of our dates of interest

dailypnlminustcost=dailypnl - smartsum(abs(weights-lag1(weights)), 2).*onewaytcost; % transaction costs are only incurred when the weights change

sharpeminustcost=sqrt(252)*smartmean(dailypnlminustcost, 1)/smartstd(dailypnlminustcost, 1) % Sharpe ratio should be about -3.19
