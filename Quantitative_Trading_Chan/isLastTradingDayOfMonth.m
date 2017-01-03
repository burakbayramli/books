function isLastTradingDayOfMonth=isLastTradingDayOfMonth(tday)
% isLastTradingDayOfMonth=isLastTradingDayOfMonth(tday) returns a logical
% array. True if tday(t) is last trading day of month.
% 
% written by:
% Ernest Chan
%
% Author of “Quantitative Trading: 
% How to Start Your Own Algorithmic Trading Business”
%
% ernest@epchan.com
% www.epchan.com

tdayStr=datestr(datenum(num2str(tday), 'yyyymmdd'));

todayMonth=month(tdayStr);

tmrMonth=fwdshift(1, todayMonth); % tomorrow's month

isLastTradingDayOfMonth=false(size(tday));

isLastTradingDayOfMonth(todayMonth~=tmrMonth & isfinite(todayMonth) & isfinite(tmrMonth))=true;
