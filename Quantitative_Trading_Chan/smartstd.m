function y = smartstd(x, dim)
%y = smartstd(x, dim)    std along dimension dim, ignoring NaN and Inf 
% 
% written by:
% Ernest Chan
%
% Author of “Quantitative Trading: 
% How to Start Your Own Algorithmic Trading Business”
%
% ernest@epchan.com
% www.epchan.com

hasData=isfinite(x);

x(~hasData)=0;

y=std(x);

y(all(~hasData, dim))=NaN;
