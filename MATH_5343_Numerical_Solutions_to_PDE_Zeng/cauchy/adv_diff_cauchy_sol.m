function [x, u] = adv_diff_cauchy_sol(N,K,c,nu)
  % [x, u] = adv_diff_cauchy_sol(N,K,c,nu)
  % Compute an approximation to the solution of
  %    u_t + c u_x - nu u_{xx} = 0    0 <= x, t <= 1
  %    u(0,t) = u(1,t)                0 <= t <= 1
  %    u(x,0) = max(0,1-(5*x-2.5)^2)  0 <= x <= 1
  % on a uniform grid with N sub-intervals using
  % the first K terms of the Fourier series.

  x = linspace(0,1,N+1);
  T = 1.0;
  u = (4/15) * ones(size(x));
  multip = 1.0;
  for k = 1 : K
    kpi = k*pi;
    coef = 10*cos(.4*k*pi)/kpi/kpi-25*sin(.4*k*pi)/kpi/kpi/kpi;
    u = u + multip*coef*exp(-4*nu*kpi*kpi*T)*cos(2*kpi*(x-c*T));
    multip = -multip;
  end
end
