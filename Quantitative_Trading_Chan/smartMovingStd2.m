function sd=smartMovingStd2(x, T, varargin)
% calculate standard deviation of x over T days. Expect T-1
% NaN in the beginning of the series
% [mvstd]=smartMovingStd(x, lookback, period) creates moving std of lookback
% periods. I.e. data is sampled every period.
% This version is inefficient. See implementation of smartMovingCorrcoef
% for faster implementation.

sd=NaN*ones(size(x));

if (nargin == 2)
    for t=T:size(x, 1)
        % for t=T:length(x)
        sd(t, :)=smartstd2(x(t-T+1:t, :),1);
    end
else
    period=varargin{1};
    for t=T*period:size(x, 1)
        sd(t, :)=smartstd2(x(t-T*period+1:t, :),1);
    end
end 
