function myassert(pred, str)
% MYASSERT Raise an error if the predicate is not true.
% myassert(pred, string)

if nargin<2, str = ''; end

if ~pred
  s = sprintf('assertion violated: %s', str);
  error(s);
end
