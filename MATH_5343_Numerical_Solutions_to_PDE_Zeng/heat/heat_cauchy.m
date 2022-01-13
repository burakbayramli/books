function [x, u] = heat_cauchy(N,cfl,nu,dt,dxx)
  % [x, u] = heat_cauchy(N,cfl,nu,dt,dxx)
  % Compute the solution to the Cauchy problem:
  %    u_t - nu u_{xx} = 0            0 <= x, t <= 1
  %    u(0,t) = u(1,t)                0 <= t <= 1
  %    u(x,0) = max(0,1-(5*x-2.5)^2)  0 <= x <= 1
  % using a uniform grid with N sub-intervals.
  % Other arguments:
  %  dt:  'be' | 'fe'
  %  dxx: 'cd'

  x = linspace(0,1,N+1);
  h = 1./N; % spatial interval size
  T = 1;
  % Determine the time step size by the CFL condition 
  k = cfl*.5*h*h/nu;

  % Initial condition for u
  u = max(0,1-(5*x'-2.5).^2);

  % Spatial discretization
  A = zeros(N+1);
  I = eye(N+1);
  % Setup index for periodic boundary condition
  rght = @(i)(i<N+1)*(i+1)+(i==N+1)*2;
  left = @(i)(i>1)*(i-1)+(i==1)*N;
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
  % - function handle to march in time -
  if strcmp( dt, 'fe' ) == 1
    mt = @(kval,uval)(I-kval*A)*uval;
  elseif strcmp( dt, 'be' ) == 1
    mt = @(kval,uval)(I+kval*A)\uval;
  else
    disp('The time-marching method is not supported');
  end
  % - solving the heat equation -
  finish = 0;
  while finish==0
    if k>T % Check if the remaining time is smaller than k
      k = T;
      finish = 1;
    end
    u = mt(k,u); % Marching in time
    T = T - k; % Remove time step size from remaining time.
  end
end
