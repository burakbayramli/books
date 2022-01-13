function [x,y,U] = adv2d_cauchy(N1,N2,cfl,c1,c2,dt,dx)
  % [x, y, U] = adv2d_cauchy(N1,N2,cfl,c1,c2,dt,dx)
  % Compute the solution to the Cauchy problem:
  %    u_t+ c1 u_x + c2 u_y=0         0 <= x, y, t <= 1
  %    u(0,y,t) = u(1,y,t)            0 <= y, t <= 1
  %    u(x,0,t) = u(x,1,t)            0 <= x, t <= 1
  %    u(x,y,0) = [max(0,1-(4x-2)^2-(4y-2)^2)]^2 
  %                                   0 <= x, y <= 1
  % using a uniform grid with N sub-intervals.
  % Other arguments:
  %  dt:  'be' | 'fe'
  %  dxx: 'upw'

  x = linspace(0,1,N1+1);
  y = linspace(0,1,N2+1);
  h1 = 1./N1;
  h2 = 1./N2;
  T = 1;
  % Determine the time step size by the CFL condition 
  k = cfl*1/(abs(c1)/h1+abs(c2)/h2);

  % Index transformation from 2D to 1D: i,j -> J
  idx = @(i,j)((i-1)*(N2+1)+j);

  % Create a one-dimensional array for the 2D initial data
  u = zeros((N1+1)*(N2+1),1);
  for i = 1:N1+1
    for j = 1:N2+1
      u(idx(i,j)) = (max(0,1-(4*x(i)-2)^2-(4*y(j)-2)^2))^2;
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
  if strcmp( dx, 'upw' ) == 1
    if c1 > 0.0
      for i = 1 : N1+1
        for j = 1 : N2+1
          J = idx(i,j); J_left = idx(left(i),j);
          A(J,J_left) = A(J,J_left) - c1/h1;
          A(J,J)      = A(J,J) + c1/h1;
        end
      end
    else
      for i = 1 : N1+1
        for j = 1 : N2+1
          J = idx(i,j); J_rght = idx(rght(i),j);
          A(J,J)      = A(J,J) - c1/h1;
          A(J,J_rght) = A(J,J_rght) + c1/h1;
        end
      end
    end
    if c2 > 0.0
      for i = 1 : N1+1
        for j = 1 : N2+1
          J = idx(i,j); J_belw = idx(i,belw(j));
          A(J,J_belw) = A(J,J_belw) - c2/h2;
          A(J,J)      = A(J,J) + c2/h2;
        end
      end
    else
      for i = 1 : N1+1
        for j = 1 : N2+1
          J = idx(i,j); J_abve = idx(i,abve(j));
          A(J,J)      = A(J,J) - c2/h2;
          A(J,J_abve) = A(J,J_abve) + c2/h2;
        end
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

  % - rearrange the data into a 2D-array
  U = zeros(N1+1,N2+1);
  for i = 1:N1+1
    for j = 1:N2+1
      U(i,j) = u(idx(i,j));
    end
  end
end
