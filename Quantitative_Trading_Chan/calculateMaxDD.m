function [maxDD maxDDD]=calculateMaxDD(cumret)
% [maxDD maxDDD]=calculateMaxDD(cumret)
% calculation of maximum drawdown and maximum drawdown duration based on
% cumulative COMPOUNDED returns. cumret must be a compounded cumulative return.
% Same as calculateMaxDD_cpd for backward compatibility.
% written by:
% Ernest Chan
%
% Author of “Quantitative Trading: 
% How to Start Your Own Algorithmic Trading Business”
%
% ernest@epchan.com
% www.epchan.com

highwatermark=zeros(size(cumret)); % initialize high watermarks to zero.

drawdown=zeros(size(cumret)); % initialize drawdowns to zero.

drawdownduration=zeros(size(cumret)); % initialize drawdown duration to zero.

for t=2:length(cumret)
    highwatermark(t)=max(highwatermark(t-1), cumret(t));
    drawdown(t)=(1+cumret(t))./(1+highwatermark(t))-1; % drawdown on each day
    if (drawdown(t)==0)
        drawdownduration(t)=0;
    else
        drawdownduration(t)=drawdownduration(t-1)+1;
    end
end

maxDD=min(drawdown); % maximum drawdown

maxDDD=max(drawdownduration); % maximum drawdown duration
