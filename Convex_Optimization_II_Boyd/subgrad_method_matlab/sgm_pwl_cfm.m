function [x,hist] = sgm_pwl_cfm(A,b,x1,fmin,MAX_ITERS)
%********************************************************************
% subgradient method for linear piecewise minimization
% uses step size rule as proposed by Camerini, Fratta, and Maffioli
%********************************************************************
f = [+Inf]; fbest = [+Inf];

k = 1;
x = x1;
sprev = zeros(length(x),1);

while k < MAX_ITERS 

  % subgradient calculation
  [fval,ind] = max(A*x+b);
  g = A(ind,:)';

  % step size selection
  beta = max(0, -(1.5)*(sprev'*g)/norm(sprev)^2);
  s = g + beta*sprev;
  alpha = (fval - fmin)/norm(s)^2;

  % objective values
  f(end+1) = fval;
  fbest(end+1) = min( fval, fbest(end) );

  % subgradient update
  x = x - alpha*s;  k = k + 1;
  sprev = s;
end

% collect history information
hist{1} = f; hist{2} = fbest;
