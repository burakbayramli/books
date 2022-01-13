function [x, u] = adv_cauchy(N,cfl,c,dt,dx,fic)
  % [x, u] = adv_cauchy(N,cfl,c,dt,dx,ic)
  % Compute the solution to the Cauchy problem:
  %    u_t + cu u_x = 0               0 <= x, t <= 1
  %    u(0,t) = u(1,t)                0 <= t <= 1
  %    u(x,0) is specified by the input "fic".
  % using a uniform grid with N sub-intervals.
  % Other arguments:
  %  dt:  'be' | 'fe' | 'cn'
  %  dxx: 'cd' | 'upw'

  x = linspace(0,1,N+1);
  h = 1./N; % spatial interval size
  T = 1;
  % Determine the time step size by the CFL condition 
  k = cfl*h/abs(c);

  % fic should be a function handle
  u = fic(x');

  % Spatial discretization
  A = zeros(N+1);
  I = eye(N+1);
  % Setup index for periodic boundary condition
  rght = @(i)(i<N+1)*(i+1)+(i==N+1)*2;
  left = @(i)(i>1)*(i-1)+(i==1)*N;
  % - discretization of advection part -
  if strcmp( dx, 'cd' ) == 1
    for i = 1 : N+1
      A(i,left(i)) = A(i,left(i)) - .5*c/h;
      A(i,rght(i)) = A(i,rght(i)) + .5*c/h;
    end
  elseif strcmp( dx, 'upw' ) == 1
    if c<0
      for i = 1 : N+1
        A(i,i)       = A(i,i) - c/h;
        A(i,rght(i)) = A(i,rght(i)) + c/h;
      end
    else
      for i = 1 : N+1
        A(i,left(i)) = A(i,left(i)) - c/h;
        A(i,i)       = A(i,i) + c/h;
      end
    end
  else
    disp('The advection part is not supported');
  end
  % - function handle to march in time -
  if strcmp( dt, 'fe' ) == 1
    mt = @(kval,uval)(I-kval*A)*uval;
  elseif strcmp( dt, 'be' ) == 1
    mt = @(kval,uval)(I+kval*A)\uval;
  elseif strcmp( dt, 'cn' ) == 1
    mt = @(kval,uval)(I+.5*kval*A)\((I-.5*kval*A)*uval);
  else
    disp('The time-marching method is not supported');
  end
  % - solving the advection equation -
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
