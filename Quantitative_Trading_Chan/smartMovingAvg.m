function [mvavg] = smartMovingAvg(x, T, varargin)
% [mvavg]=movingAvg(x, lookback). create moving average series over T days. mvavg
% has T-1 NaN in beginning. Ignore over days with NaN.
% [mvavg]=movingAvg(x, lookback, period) creates moving avg of lookback
% periods. I.e. data is sampled every period.

assert(T>0);

mvavg=zeros(size(x));

goodDays=isfinite(x);

xx=x;
xx(~goodDays)=0;

numGoodDays=zeros(size(x));

if (nargin == 2)
    for i=0:T-1
        mvavg=mvavg+backshift(i, xx);
        numGoodDays=numGoodDays+isfinite(backshift(i, x));
    end
else
    period=varargin{1};
    for i=0:T-1
        mvavg=mvavg+backshift(i*period, xx);
        numGoodDays=numGoodDays+isfinite(backshift(i*period, x));
    end
end
    
nonzeroDays=numGoodDays>0;
mvavg(nonzeroDays)=mvavg(nonzeroDays) ./ numGoodDays(nonzeroDays);
mvavg(~nonzeroDays)=NaN;


