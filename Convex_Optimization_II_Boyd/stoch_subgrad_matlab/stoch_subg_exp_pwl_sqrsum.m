function [x,hist] = stoch_subg_exp_pwl_sqrsum(Abar,bbar,K,x1,MAX_ITERS)
%********************************************************************
% stochastic subgradient method for expected piecewise linear problem
% uses square summable, but nonsummable step size rule, alpha_k = a/k
%********************************************************************
f = [+Inf]; fbest = [+Inf]; fbest_nom = [+Inf];
[m,n] = size(Abar);

sigma_A = sqrt(5);
sigma_b = sqrt(5);

iter = 1;
x = x1;
xhist = [x];

while iter < MAX_ITERS 
  if( rem(iter,500) == 0 ), fprintf(1,'iter: %d\n',iter), end

  % noisy subgradient calculation
  gsum = zeros(n,1);
  for k = 1:K
    % generate a random realization of the matrix A
    A = Abar + sigma_A/sqrt(m)*randn([m n]);
    b = bbar + sigma_b/sqrt(m)*randn([m 1]);

    [fval,ind] = max(A*x+b);
    gsum = gsum + A(ind,:)';
  end
  g = (1/K)*gsum;

  % step size selection
  alpha = 1/iter;

  % objective values
  f(end+1) = fval;
  fbest(end+1) = min( fval, fbest(end) );
  fbest_nom(end+1) = min( max(Abar*x + bbar) , fbest_nom(end) );

  % subgradient update
  x = x - alpha*g; iter = iter + 1; xhist = [xhist, x];
end

% collect history information
hist{1} = fbest_nom; hist{2} = fbest; hist{3} = xhist;
