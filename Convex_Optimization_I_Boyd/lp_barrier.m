function [x_star, history, gap] = lp_barrier(A,b,c,x_0)
  T_0 = 1;
  MU = 20;
  EPSILON = 1e-3;

  n = length(x_0);
  t = T_0;
  x = x_0;
  history = [];

  while(1)
    [x_star, nu_star, lambda_hist] = lp_acent(A,b,t*c,x);
    x = x_star;
    gap = n/t;
    history = [history [length(lambda_hist); gap]];
    if gap < EPSILON break; end
    t = MU*t;
  end
