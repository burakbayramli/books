function [x,hist] = sgm_pwl_optimal_step(A,b,x1,fmin,MAX_ITERS)
%********************************************************************
% subgradient method for linear piecewise minimization
% uses Polyak's optimal step size based on knowledge of optimal value
%********************************************************************
f = [+Inf]; fbest = [+Inf];

k = 1;
x = x1;

while k < MAX_ITERS 

  % subgradient calculation
  [fval,ind] = max(A*x+b);
  g = A(ind,:)';

  % step size selection
  alpha = (fval-fmin)/norm(g)^2;

  % objective values
  f(end+1) = fval;
  fbest(end+1) = min( fval, fbest(end) );

  % subgradient update
  x = x - alpha*g;  k = k + 1;
end

% collect history information
hist{1} = f; hist{2} = fbest;
