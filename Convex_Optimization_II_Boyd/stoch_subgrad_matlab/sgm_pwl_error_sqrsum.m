function [x,hist] = stoch_sgm_pwl_sqrsum(A,b,x1,a,sigmasqr,MAX_ITERS)
%********************************************************************
% subgradient method for linear piecewise minimization
% uses square summable, but nonsummable step size rule, alpha_k = a/k
% however, there are errors in the subgradient evaluations
%
% EE364b Convex Optimization II, S. Boyd
% Written by Almir Mutapcic, 01/19/07
%********************************************************************
f = [+Inf]; fbest = [+Inf];

[m,n] = size(A);

k = 1;
x = x1;

while k < MAX_ITERS 

  % subgradient calculation
  [fval,ind] = max(A*x+b);
  g = A(ind,:)';

  % add noise to the subgradient (noisy subgradient)
  g = g + sqrt(sigmasqr)*randn([n 1]);

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
