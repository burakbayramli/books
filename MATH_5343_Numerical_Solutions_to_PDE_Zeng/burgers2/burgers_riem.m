function [x, u] = burgers_riem(N,cfl,nf,x_l,x_r,T,u_l,u_r)
  % [x, u] = burgers_fvm(N,cfl,nf,x_l,x_r,T,u_l,u_r)
  % Compute the solution to the Riemann problem of the 
  % Burgers equation
  %   u_t + (u^2/2)_x = 0  
  % on the domain x_l <= x <= x_r, 0 <= t <= T
  % using a uniform grid with N sub-intervals.
  % The initial condition is given by:
  %   u = u_l if x < 0
  %   u = u_r if x > 0
  % nf is an option for numerical flux: 'Roe', or 'Godunov'

  x = linspace(x_l,x_r,N+1);
  x = (x(2:end)+x(1:end-1))/2;
  h = (x_r-x_l)./N;

  flux = @(w).5*w*w;
  if strcmp( nf, 'Roe' ) == 1
    % An external function in the same folder
    nflux = @(u_mnus,u_plus)burgers_nf_roe(u_mnus,u_plus);
    % Used to compute the time step size k
    vmax = @(u_mnus,u_plus)max(0.5*abs(u_mnus+u_plus),eps);
  elseif strcmp( nf, 'Godunov' ) == 1
    % An external function in the same folder
    nflux = @(u_mnus,u_plus)burgers_nf_godunov(u_mnus,u_plus);
    % Used to compute the time step size k
    vmax = @(u_mnus,u_plus)max([abs(u_mnus) abs(u_plus) eps]);
  else
    disp('The numerical flux is not defined');
  end

  % Initial condition
  u = u_l*(x<0) + u_r*(x>=0);
  F = zeros(N+1,1);
  u_next = zeros(size(u));
  % Solving the Burgers equation
  finish = 0;
  % Beginning time step size
  k = cfl * h / max(abs(u_l),abs(u_r));
  for i = 2:N
    k = min(k,cfl*h/vmax(u(i-1),u(i)));
  end
  while finish==0
    if k > T % Check if the remaining time is smaller than k
      k = T;
      finish = 1;
    end
    % Compute the numerical flux
    F(1) = flux(u_l); F(end) = flux(u_r);
    % Compute the fluxes
    for i = 2:N
      F(i) = nflux(u(i-1),u(i));
    end
    % Update the solutions
    for i = 1:N
      u_next(i) = u(i) - (k/h) * (F(i+1)-F(i));
    end
    % Compute the next time step size
    u = u_next;
    k = cfl * h / max(abs(u_l),abs(u_r));
    for i = 2:N
      k = min(k,cfl*h/vmax(u(i-1),u(i)));
    end
    T = T - k; % Remove time step size from remaining time.
    disp(sprintf('Remaining time: %f.\n', T));
  end
end
