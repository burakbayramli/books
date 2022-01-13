function [x,y,U] = heat2d_cauchy(N1,N2,cfl,nu1,nu2,dt,dxx)
  % [x, y, U] = heat2d_cauchy(N1,N2,cfl,nu1,nu2,dt,dxx)
  % Compute the solution to the Cauchy problem:
  %    u_t-nu1 u_{xx}-nu2 u_{yy}=0    0 <= x, y, t <= 1
  %    u(0,y,t) = u(1,y,t)            0 <= y, t <= 1
  %    u(x,0,t) = u(x,1,t)            0 <= x, t <= 1
  %    u(x,y,0) = max(0,1-(5*x-2.5)^2-(5*y-2.5)^2)  
  %                                   0 <= x, y <= 1
  % using a uniform grid with N sub-intervals.
  % Other arguments:
  %  dt:  'be' | 'fe'
  %  dxx: 'cd'

  x = linspace(0,1,N1+1);
  y = linspace(0,1,N2+1);
  h1 = 1./N1;
  h2 = 1./N2;
  T = 1;
  % Determine the time step size by the CFL condition 
  k = cfl*.5/(nu1/h1/h1+nu2/h2/h2);

  % Index transformation from 2D to 1D: i,j -> J
  idx = @(i,j)((i-1)*(N2+1)+j);

  % Create a one-dimensional array for the 2D initial data
  u = zeros((N1+1)*(N2+1),1);
  for i = 1:N1+1
    for j = 1:N2+1
      u(idx(i,j)) = max(0,1-(5*x(i)-2.5)^2-(5*y(j)-2.5)^2);
    end
  end

  % Spatial discretization in x
  A = zeros((N1+1)*(N2+1));
  I = eye((N1+1)*(N2+1));
  % Setup index for periodic boundary condition
  rght = @(i)(i<N1+1)*(i+1)+(i==N1+1)*2;
  left = @(i)(i>1)*(i-1)+(i==1)*N1;
  abve = @(j)(j<N2+1)*(j+1)+(j==N2+1)*2;
  belw = @(j)(j>1)*(j-1)+(j==1)*N2;
  % - discretization of diffusion part -
  if strcmp( dxx, 'cd' ) == 1
    for i = 1 : N1+1
      for j = 1 : N2+1
        J = idx(i,j); 
        J_rght = idx(rght(i),j); J_left = idx(left(i),j);
        J_abve = idx(i,abve(j)); J_belw = idx(i,belw(j));
        A(J,J)      = A(J,J) + 2.*nu1/h1/h1 + 2.*nu2/h2/h2;
        A(J,J_left) = A(J,J_left) - nu1/h1/h1;
        A(J,J_rght) = A(J,J_rght) - nu1/h1/h1;
        A(J,J_belw) = A(J,J_belw) - nu2/h2/h2;
        A(J,J_abve) = A(J,J_abve) - nu2/h2/h2;
      end
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

  % - rearrange the data into a 2D-array
  U = zeros(N1+1,N2+1);
  for i = 1:N1+1
    for j = 1:N2+1
      U(i,j) = u(idx(i,j));
    end
  end
end
