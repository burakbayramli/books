function y=lag(x)
% y=lag(x)
% 
% written by:
% Ernest Chan
%
% Author of “Quantitative Trading: 
% How to Start Your Own Algorithmic Trading Business”
%
% ernest@epchan.com
% www.epchan.com

if (isnumeric(x))
    y=[NaN(1,size(x,2));x(1:end-1, :)]; % populate the first entry with NaN
elseif (ischar(x))
    y=[repmat('',[1 size(x,2)]);x(1:end-1, :)]; % populate the first entry with ''
else
    error('Can only be numeric or char array');
end