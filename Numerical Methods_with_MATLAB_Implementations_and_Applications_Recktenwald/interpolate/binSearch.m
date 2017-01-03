function ia = binSearch(x,xhat)
% binSearch  Binary search to find index i such that x(i)<= xhat <= x(i+1)
%
% Synopsis:  i = binSearch(x,xhat)
%
% Input:     x    = vector of monotonic data
%            xhat = test value
%
% Output:    i = index in x vector such that x(i)<= xhat <= x(i+1)

n = length(x);
if xhat<x(1) | xhat>x(n)
   error(sprintf('Test value of %g is not in range of x',xhat));
end

ia = 1;  ib = n;         %  Initialize lower and upper limits 
while ib-ia>1
  im = fix((ia+ib)/2);   %  Integer value of midpoint
  if x(im) < xhat
    ia = im;             %  Replace lower bracket
  else
    ib = im;             %  Replace upper bracket
  end
end                      %  When while test is true, ia is desired index
