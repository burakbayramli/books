function [mvavg] = movingAvg(x, T)
% [mvavg]=movingAvg(x, T). create moving average series over T days. mvavg
% has T-1 NaN in beginning.
% 
% written by:
% Ernest Chan
%
% Author of “Quantitative Trading: 
% How to Start Your Own Algorithmic Trading Business”
%
% ernest@epchan.com
% www.epchan.com

mvavg = zeros(size(x,1)-T+1, size(x, 2));

for i=0:T-1
    mvavg = mvavg + x(1+i:end-T+1+i, :);
end

mvavg = mvavg / T;

mvavg=[NaN*ones(T-1, size(x,2)); mvavg];


