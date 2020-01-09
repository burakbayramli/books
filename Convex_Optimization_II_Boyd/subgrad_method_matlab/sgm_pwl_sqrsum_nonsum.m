function [x,hist] = sgm_pwl_sqrsum_nonsum(A,b,x1,a,MAX_ITERS)
%********************************************************************
% subgradient method for linear piecewise minimization
% uses square summable, but nonsummable step size rule, alpha_k = a/k
%********************************************************************
f = [+Inf]; fbest = [+Inf];

k = 1;
x = x1;

while k < MAX_ITERS 

  % subgradient calculation
  [fval,ind] = max(A*x+b);
  g = A(ind,:)';

  % step size selection
  alpha = a/k;

  % objective values
  f(end+1) = fval;
  fbest(end+1) = min( fval, fbest(end) );

  % subgradient update
  x = x - alpha*g;  k = k + 1;
end

% collect history information
hist{1} = f; hist{2} = fbest;
