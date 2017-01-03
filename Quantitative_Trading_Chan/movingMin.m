function [mvmin] = movingMin(x, T)
% [mvmin]=movingMin(x, T). create moving minimum series over T days. mvavg
% has T-1 NaN in beginning. Ignore over days with NaN.
% 
% written by:
% Ernest Chan
%
% Author of “Quantitative Trading: 
% How to Start Your Own Algorithmic Trading Business”
%
% ernest@epchan.com
% www.epchan.com

mvmin = NaN(size(x));

for t=T:size(x, 1)
    mvmin(t, :) = min(x(t-T+1:t, :), [], 1);
end