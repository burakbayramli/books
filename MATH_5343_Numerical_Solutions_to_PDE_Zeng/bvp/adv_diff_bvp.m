function [x, u] = adv_diff_bvp(N,c,nu,dx,dxx)
  % [x, u] = adv_diff_bvp(N,c,nu,dx,dxx)
  % Compute the solution to the boundary value problem:
  %    c u_x - nu u_{xx} = 0    0 <= x <= 1
  %    u(0) = 0, u(1) = 1
  % using a uniform grid with N sub-intervals.
  % Other arguments:
  %  dx:  'bw' | 'fw' | 'cd'
  %  dxx: 'cd'

  A = zeros(N+1);
  b = zeros(N+1,1);
  x = linspace(0,1,N+1);
  h = 1./N;
  % Left boundary
  A(1,1) = 1;
  b(1) = 0;
  % right boundary
  A(end,end) = 1;
  b(end) = 1;
  % Interior points
  % - discretization of advection part -
  if strcmp( dx, 'bw' ) == 1
    for i = 2 : N
      A(i,i-1) = A(i,i-1) - c/h;
      A(i,i)   = A(i,i) + c/h;
    end
  elseif strcmp( dx, 'cd' ) == 1
    for i = 2 : N
      A(i,i-1) = A(i,i-1) - .5*c/h;
      A(i,i+1) = A(i,i+1) + .5*c/h;
    end
  elseif strcmp( dx, 'fw' ) == 1
    for i = 2 : N
      A(i,i)   = A(i,i) - c/h;
      A(i,i+1) = A(i,i+1) + c/h;
    end
  else
    disp('The advection part is not supported');
  end
  % - discretization of diffusion part -
  if strcmp( dxx, 'cd' ) == 1
    for i = 2 : N
      A(i,i-1) = A(i,i-1) - nu/h/h;
      A(i,i)   = A(i,i) + 2.*nu/h/h;
      A(i,i+1) = A(i,i+1) - nu/h/h;
    end
  else
    disp('The diffusion part is not supported');
  end
  % Find the solution
  u = A\b;
end
