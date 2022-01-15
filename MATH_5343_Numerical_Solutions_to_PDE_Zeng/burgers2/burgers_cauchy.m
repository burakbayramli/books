function [x, u] = burgers_cauchy(N,cfl,nf,fic,T)
  % [x, u] = burgers_cauchy(N,cfl,nf,fic,T)
  % Compute the solution to the Riemann problem of the 
  % Burgers equation
  %   u_t + (u^2/2)_x = 0  
  % on the domain 0 <= x <= 1, 0 <= t <= T
  % using a uniform grid with N sub-intervals.
  % The initial condition is given by the function fic.
  % nf is an option for numerical flux: 'Roe', or 'Godunov'

  x = linspace(0,1,N+1);
  x = (x(2:end)+x(1:end-1))/2;
  h = 1/N;

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
  u = fic(x);
  F = zeros(N+1,1);
  u_next = zeros(size(u));
  % Setup index for periodic boundary condition
  % i denotes the index for F, from 1 to N+1
  % the function returns the index for cell
  rght = @(i)(i<N+1)*i+(i==N+1)*1;
  left = @(i)(i==1)*N+(i>1)*(i-1);
  % Solving the Burgers equation
  finish = 0;
  % Beginning time step size
  k = realmax;
  for i = 1:N+1
    k = min(k,cfl*h/vmax(u(left(i)),u(rght(i))));
  end
  while finish==0
    if k > T % Check if the remaining time is smaller than k
      k = T;
      finish = 1;
    end
    % Compute the numerical flux
    for i = 1:N+1
      F(i) = nflux(u(left(i)),u(rght(i)));
    end
    % Update the solutions
    for i = 1:N
      u_next(i) = u(i) - (k/h) * (F(i+1)-F(i));
    end
    % Compute the next time step size
    u = u_next;
    k = realmax;
    for i = 1:N+1
      k = min(k,cfl*h/vmax(u(left(i)),u(rght(i))));
    end
    T = T - k; % Remove time step size from remaining time.
    disp(sprintf('Remaining time: %f.\n', T));
  end
end
