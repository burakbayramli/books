function [x, u] = adv_cauchy_lf(N,cfl,c,fic)
  % [x, u] = adv_cauchy_lf(N,cfl,c,ic)
  % Compute the solution to the Cauchy problem:
  %    u_t + cu u_x = 0               0 <= x, t <= 1
  %    u(0,t) = u(1,t)                0 <= t <= 1
  %    u(x,0) is specified by the input "fic".
  % using a uniform grid with N sub-intervals and
  % the Lax-Friedrich method.

  x = linspace(0,1,N+1);
  h = 1./N; % spatial interval size
  T = 1;
  % Determine the time step size by the CFL condition 
  k = cfl*h/abs(c);

  % fic should be a function handle
  u = fic(x');

  % Spatial discretization u^{n+1} = (D-kA) u^n
  D = zeros(N+1);
  A = zeros(N+1);
  % Setup index for periodic boundary condition
  rght = @(i)(i<N+1)*(i+1)+(i==N+1)*2;
  left = @(i)(i>1)*(i-1)+(i==1)*N;
  for i = 1 : N+1
    D(i,left(i)) = .5;
    D(i,rght(i)) = .5;
  end
  for i = 1 : N+1
    A(i,left(i)) = A(i,left(i)) - .5*c/h;
    A(i,rght(i)) = A(i,rght(i)) + .5*c/h;
  end
  % - solving the advection equation -
  finish = 0;
  while finish==0
    if k>T % Check if the remaining time is smaller than k
      k = T;
      finish = 1;
    end
    u = (D-k*A)*u;
    T = T - k; % Remove time step size from remaining time.
  end
end
