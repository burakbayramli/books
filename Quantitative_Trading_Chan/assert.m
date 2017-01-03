function assert(pred, str)
% ASSERT Raise an error if the predicate is not true.
% assert(pred, string)
%
% written by:
% Ernest Chan
%
% Author of “Quantitative Trading: 
% How to Start Your Own Algorithmic Trading Business”
%
% ernest@epchan.com
% www.epchan.com

if nargin<2, str = ''; end

if ~pred
  s = sprintf('assertion violated: %s', str);
  error(s);
end
