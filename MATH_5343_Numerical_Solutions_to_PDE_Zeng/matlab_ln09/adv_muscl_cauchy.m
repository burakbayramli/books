function [x, u] = adv_muscl_cauchy(N,cfl,c,dt,flx,lim,fic,T)
  % [x, u] = adv_muscl_cauchy(N,cfl,c,dt,lim,fic,T)
  % Compute the solution to the Riemann problem of the 
  % Burgers equation
  %   u_t + c u_x = 0  
  % on the domain 0 <= x <= 1, 0 <= t <= T
  % using a uniform grid with N sub-intervals.
  % The initial condition is given by the function fic.
  % Other arguments:
  %  dt:  'fe' | 'rk2'
  %  flx: 'rea' | 'osher'
  %  lim: 'zero' | 'one' | 'minmod' | 'superbee' |
  %       'mc' | 'vanleer' | 'vanalbada'

  x = linspace(0,1,N+1);
  x = (x(2:end)+x(1:end-1))/2;
  h = 1/N;

  if strcmp( lim, 'zero' ) == 1
    nslope = @(h,u_mnus,u_this,u_plus)lim_zero(h,u_mnus,u_this,u_plus);
  elseif strcmp( lim, 'one' ) == 1
    nslope = @(h,u_mnus,u_this,u_plus)lim_one(h,u_mnus,u_this,u_plus);
  elseif strcmp( lim, 'minmod' ) == 1
    nslope = @(h,u_mnus,u_this,u_plus)lim_minmod(h,u_mnus,u_this,u_plus);
  elseif strcmp( lim, 'superbee' ) == 1
    nslope = @(h,u_mnus,u_this,u_plus)lim_superbee(h,u_mnus,u_this,u_plus);
  elseif strcmp( lim, 'mc' ) == 1
    nslope = @(h,u_mnus,u_this,u_plus)lim_mc(h,u_mnus,u_this,u_plus);
  elseif strcmp( lim, 'vanleer' ) == 1
    nslope = @(h,u_mnus,u_this,u_plus)lim_vanleer(h,u_mnus,u_this,u_plus);
  elseif strcmp( lim, 'vanalbada' ) == 1
    nslope = @(h,u_mnus,u_this,u_plus)lim_vanalbada(h,u_mnus,u_this,u_plus);
  else
    disp('The slope limiter is not defined');
  end

  if strcmp( flx, 'rea' ) == 1
    f_adv = @(k,h,u)adv_cauchy_rea_fe( N, c, k, h, nslope, u );
  elseif strcmp( flx, 'osher' ) == 1
    f_adv = @(k,h,u)adv_cauchy_osher_fe( N, c, k, h, nslope, u );
  else
    disp('The numerical flux is not defined');
  end

  % Initial condition
  u = fic(x);
  % Solving the advection equation
  finish = 0;
  % Beginning time step size
  k = cfl*h/abs(c);
  while finish==0
    if k > T % Check if the remaining time is smaller than k
      k = T;
      finish = 1;
    end
    if strcmp( dt, 'fe' ) == 1
      u_next = f_adv( k, h, u );
    elseif strcmp( dt, 'rk2' ) == 1
      u_stg1 = f_adv( k, h, u );
      u_stg2 = f_adv( k, h, u_stg1 );
      u_next = (u+u_stg2)/2;
    else
      disp('The time integrator is not defined');
    end
    u = u_next;
    T = T - k; % Remove time step size from remaining time.
  end
end
