function sd=movingStd(x, T, varargin)
% calculate standard deviation of x over T days. Expect T-1
% NaN in the beginning of the series
% [mvstd]=movingStd(x, lookback, period) creates moving std of lookback
% periods. I.e. data is sampled every period.
% This uses std which normalizes by N-1.

sd=NaN*ones(size(x));

if (nargin == 2)
    for t=T:size(x, 1)
        % for t=T:length(x)
        sd(t, :)=std(x(t-T+1:t, :));
    end
else
    period=varargin{1};
    for t=T*period:size(x, 1)
        sd(t, :)=std(x(t-T*period+1:t, :));
    end
end 
