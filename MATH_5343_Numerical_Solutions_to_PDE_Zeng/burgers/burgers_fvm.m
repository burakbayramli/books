function [x, t, U] = burgers_fvm(N,cfl,x_l,x_r,T,u_l,u_r)
  % [x, t, u] = burgers_fvm(N,cfl,x_l,x_r,T,u_l,u_r)
  % Compute the solution to the Burgers equation
  %  u_t + uu_x = 0  
  % on the domain x_l <= x <= x_r, 0 <= t <= T
  % using a uniform grid with N sub-intervals

  x = linspace(x_l,x_r,N+1);
  x = (x(2:end)+x(1:end-1))/2;
  h = (x_r-x_l)./N;

  flux = @(w).5*w*w;
  roe  = @(w_l,w_r).5*(w_l+w_r);
  
  % Initial condition
  u = u_l*(x<0) + u_r*(x>=0);
  F = zeros(N+1,1);
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
    % Compute the numerical flux
    F(1) = flux(u_l); F(end) = flux(u_r);
    % Compute the fluxes
    for i = 2:N
      F(i) = .5*flux(u(i-1))+.5*flux(u(i))-.5*abs(roe(u(i-1),u(i)))*(u(i)-u(i-1));
    end
    % Update the solutions
    for i = 1:N
      u_next(i) = u(i) - (k/h) * (F(i+1)-F(i));
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
