function [x,hist] = sgm_pwl_const_step_length(A,b,x1,R,gamma,TOL,MAX_ITERS)
%********************************************************************
% subgradient method for piecewise linear minimization
% - uses constant step length rule, alpha_k = gamma/norm(subgrad_k);
% - keeps track of function and best function values
% - also computes the best lower bound (which is a very loose bound)
%
% EE364b Convex Optimization II, S. Boyd
% Written by Almir Mutapcic, 01/19/07
%********************************************************************
f = [+Inf]; fbest = [+Inf]; lbest = [-Inf];
sum_alpha = 0; sum_alpha_f = 0; sum_alpha_subg = 0;

k = 1;
x = x1;

while k < MAX_ITERS 

  % subgradient calculation
  [fval,ind] = max(A*x+b);
  g = A(ind,:)';

  % step size selection
  alpha = gamma/norm(g);

  % objective values
  f(end+1) = fval;
  fbest(end+1) = min( fval, fbest(end) );

  % lower bound computation
  sum_alpha      = sum_alpha + alpha;
  sum_alpha_f    = sum_alpha_f + alpha*fval;
  sum_alpha_subg = sum_alpha_subg + alpha^2*norm(g)^2;

  lval = (2*sum_alpha_f - R^2 - sum_alpha_subg)/(2*sum_alpha); 
  lbest(end+1) = max( lval, lbest(end) );

  % stopping criteria
  if( fbest(end) - lbest(end) < TOL ), break, end;

  % subgradient update
  x = x - alpha*g;  k = k + 1;
end

% collect history information
hist{1} = f; hist{2} = fbest; hist{3} = lbest;
