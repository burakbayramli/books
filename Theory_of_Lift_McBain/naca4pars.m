function [m, p, t] = naca4pars (n)
  m = 0.01 * round (n / 1e3);
  p = 0.1 * round (rem (n, 1e3) / 1e2);
  t = 0.01 * rem (n, 1e2);
