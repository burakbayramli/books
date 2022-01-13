function [x, u] = adv_diff_cauchy_midpoint(N,M,c,nu)
  % [x, u] = adv_diff_cauchy_midtime(N,c,nu,dx,dxx)
  % Compute the solution to the Cauchy problem:
  %    u_t + c u_x - nu u_{xx} = 0    0 <= x, t <= 1
  %    u(0,t) = u(1,t)                0 <= t <= 1
  %    u(x,0) = max(0,1-(5*x-2.5)^2)  0 <= x <= 1
  % using a uniform grid with N sub-intervals.

  x = linspace(0,1,N+1);
  h = 1./N; % spatial interval size
  T = 1;
  k = T./M; % time step size

  % Initial condition for u
  u = max(0,1-(5*x'-2.5).^2);

  % The method updating from u0 to u1 looks like
  % (u1 - u0)/k + A (u1 + u0)/2 = 0
  A = zeros(N+1);
  I = eye(N+1);
  % Setup index for periodic boundary condition
  rght = @(i)(i<N+1)*(i+1)+(i==N+1)*2;
  left = @(i)(i>1)*(i-1)+(i==1)*N;
  % - discretization of advection part -
  for i = 1 : N+1
    A(i,left(i)) = A(i,left(i)) - .5*c/h;
    A(i,rght(i)) = A(i,rght(i)) + .5*c/h;
  end
  % - discretization of diffusion part -
  for i = 1 : N+1
    A(i,left(i)) = A(i,left(i)) - nu/h/h;
    A(i,i)       = A(i,i) + 2.*nu/h/h;
    A(i,rght(i)) = A(i,rght(i)) - nu/h/h;
  end
  % - marching in time -
  for n = 1:M
    u = (I+.5*k*A)\((I-.5*k*A)*u);
  end
end
