function [x,hist] = sgm_pwl_momentum(A,b,x1,beta,MAX_ITERS)
%********************************************************************
% subgradient method for linear piecewise minimization
% uses `heavy-ball' or `momentum' step size
%********************************************************************
f = [+Inf]; fbest = [+Inf];

k = 1;
x = x1;
xprev = zeros(length(x),1);

while k < MAX_ITERS 

  % subgradient calculation
  [fval,ind] = max(A*x+b);
  g = A(ind,:)';

  % step size selection
  alpha = 1/k;

  % objective values
  f(end+1) = fval;
  fbest(end+1) = min( fval, fbest(end) );

  % subgradient update
  xnew = x - (1-beta)*alpha*g + beta*(x - xprev);  k = k + 1;
  xprev = x;
  x = xnew;
end

% collect history information
hist{1} = f; hist{2} = fbest;
