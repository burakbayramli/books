function [x, t, U] = burgers_fdm(N,cfl,x_l,x_r,T,u_l,u_r)
  % [x, t, u] = burgers_fdm(N,cfl,x_l,x_r,T,u_l,u_r)
  % Compute the solution to the Burgers equation
  %  u_t + uu_x = 0  
  % on the domain x_l <= x <= x_r, 0 <= t <= T
  % using a uniform grid with N sub-intervals

  x = linspace(x_l,x_r,N+1);
  h = (x_r-x_l)/N;
  
  % Initial condition
  u = u_l*(x<0) + u_r*(x>=0);
  u_next = zeros(size(u));
  % Storing the solutions
  nstep = 1;
  U(nstep,:) = u; t(nstep) = 0;
  % Solving the Burgers equation
  finish = 0;
  k = cfl * h / max(abs(u));
  while finish==0
    if k > T % Check if the remaining time is smaller than k
      k = T;
      finish = 1;
    end
    % Upwind + FE to update from u to u_next
    u_next(1) = u_l; u_next(end) = u_r;
    for i = 2:N
      if u(i) > 0
        u_next(i) = u(i) - k * u(i) * ((u(i)-u(i-1))/h);
      else
        u_next(i) = u(i) - k * u(i) * ((u(i+1)-u(i))/h);
      end
    end
    % Compute the next time step size
    u = u_next;
    k = cfl * h / max(abs(u));
    T = T - k; % Remove time step size from remaining time.
    % Storing the solutions
    nstep = nstep+1;
    U(nstep,:) = u;
    t(nstep) = t(nstep-1)+k;
  end
end
