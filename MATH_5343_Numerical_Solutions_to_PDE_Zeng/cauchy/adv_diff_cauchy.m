function [x, u] = adv_diff_cauchy(N,M,c,nu,dt,dx,dxx)
  % [x, u] = adv_diff_cauchy(N,c,nu,dx,dxx)
  % Compute the solution to the Cauchy problem:
  %    u_t + c u_x - nu u_{xx} = 0    0 <= x, t <= 1
  %    u(0,t) = u(1,t)                0 <= t <= 1
  %    u(x,0) = max(0,1-(5*x-2.5)^2)  0 <= x <= 1
  % using a uniform grid with N sub-intervals.
  % Other arguments:
  %  dt:  'cd' | 'bw' | 'fw' | 'bdf2'
  %  dx:  'bw' | 'fw' | 'cd'
  %  dxx: 'cd'

  x = linspace(0,1,N+1);
  h = 1./N; % spatial interval size
  T = 1;
  k = T./M; % time step size

  % Initial condition for u
  u = max(0,1-(5*x'-2.5).^2);

  % Spatial discretization
  A = zeros(N+1);
  I = eye(N+1);
  % Setup index for periodic boundary condition
  rght = @(i)(i<N+1)*(i+1)+(i==N+1)*2;
  left = @(i)(i>1)*(i-1)+(i==1)*N;
  % - discretization of advection part -
  if strcmp( dx, 'bw' ) == 1
    for i = 1 : N+1
      A(i,left(i)) = A(i,left(i)) - c/h;
      A(i,i)       = A(i,i) + c/h;
    end
  elseif strcmp( dx, 'cd' ) == 1
    for i = 1 : N+1
      A(i,left(i)) = A(i,left(i)) - .5*c/h;
      A(i,rght(i)) = A(i,rght(i)) + .5*c/h;
    end
  elseif strcmp( dx, 'fw' ) == 1
    for i = 1 : N+1
      A(i,i)       = A(i,i) - c/h;
      A(i,rght(i)) = A(i,rght(i)) + c/h;
    end
  else
    disp('The advection part is not supported');
  end
  % - discretization of diffusion part -
  if strcmp( dxx, 'cd' ) == 1
    for i = 1 : N+1
      A(i,left(i)) = A(i,left(i)) - nu/h/h;
      A(i,i)       = A(i,i) + 2.*nu/h/h;
      A(i,rght(i)) = A(i,rght(i)) - nu/h/h;
    end
  else
    disp('The diffusion part is not supported');
  end
  % - marching in time -
  if strcmp( dt, 'fw' ) == 1
    for n = 1:M
      u = (I-k*A)*u;
    end
  elseif strcmp( dt, 'bw' ) == 1
    for n = 1:M
      u = (I+k*A)\u;
    end
  elseif strcmp( dt, 'cd' ) == 1
    % Initialization step using 'fw'
    u0 = u;           % u at n=0
    u1 = (I-k*A)*u0;  % u at n=1
    for n = 2:M
      u = u0 - 2*k*A*u1;
      u0 = u1; u1 = u;
    end
  elseif strcmp( dt, 'bdf2' ) == 1
    % Initialization step using 'bw'
    u0 = u;           % u at n=0
    u1 = (I+k*A)\u;   % u at n=1
    for n = 2:M
      u = (1.5*I+k*A)\(2*u1-.5*u0);
      u0 = u1; u1 = u;
    end
  else
    disp('The time-marching method is not supported');
  end
end
