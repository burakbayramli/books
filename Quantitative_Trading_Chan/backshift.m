function y=backshift(day,x)
% y=backshift(day,x)
% 
% written by:
% Ernest Chan
%
% Author of “Quantitative Trading: 
% How to Start Your Own Algorithmic Trading Business”
%
% ernest@epchan.com
% www.epchan.com

assert(day>=0);
y=[NaN(day,size(x,2), size(x, 3));x(1:end-day,:, :)];
