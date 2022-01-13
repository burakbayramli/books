function [x, h, u] = swe_riemann_muscl(N,cfl,vars,lim)
  % [x, h, u] = swe_riemann_muscl(N,cfl,vars,lim)
  % Using Osher's MUSCL method to solve a Riemann problem
  % for shallow water equation.
  % RK2 is used for integration in time.
  %   U_t + F(U)_x = 0
  %   U = [h; hu]
  %   F(U) = [hu; hu^2+gh^2/2]
  % h is height and u is velocity
  % N is the number of cells of a uniform grid for [-1, 1]
  % cfl is the CFL numbero
  % vars is the variable to which limited slope is computed
  %  'riem' - riemann invariants: u+2\sqrt{gh}, u-2\sqrt{gh}
  %  'csrv' - conservative variables, h and hu 
  %  'prmv' - primitive variables: h and u
  % lim is an option for limiter function: 'zero' | 'one' 
  %  | 'minmod' | 'superbee' | 'mc' | 'vanleer' | 'vanalbada'

  % Setup the grid
  x = linspace(-1,1,N+1);
  x = (x(2:end)+x(1:end-1))/2; % x is 1 X N
  dx = 2/N;
  T = 0.2;

  % Setup the limiter function
  if strcmp( lim, 'zero' ) == 1
    nslope = @(dx,u_mnus,u_this,u_plus)lim_zero(dx,u_mnus,u_this,u_plus);
  elseif strcmp( lim, 'one' ) == 1
    nslope = @(dx,u_mnus,u_this,u_plus)lim_one(dx,u_mnus,u_this,u_plus);
  elseif strcmp( lim, 'minmod' ) == 1
    nslope = @(dx,u_mnus,u_this,u_plus)lim_minmod(dx,u_mnus,u_this,u_plus);
  elseif strcmp( lim, 'superbee' ) == 1
    nslope = @(dx,u_mnus,u_this,u_plus)lim_superbee(dx,u_mnus,u_this,u_plus);
  elseif strcmp( lim, 'mc' ) == 1
    nslope = @(dx,u_mnus,u_this,u_plus)lim_mc(dx,u_mnus,u_this,u_plus);
  elseif strcmp( lim, 'vanleer' ) == 1
    nslope = @(dx,u_mnus,u_this,u_plus)lim_vanleer(dx,u_mnus,u_this,u_plus);
  elseif strcmp( lim, 'vanalbada' ) == 1
    nslope = @(dx,u_mnus,u_this,u_plus)lim_vanalbada(dx,u_mnus,u_this,u_plus);
  else
    disp('The slope limiter is not defined');
  end

  % Setup the FE solver
  if strcmp( vars, 'riem' ) == 1
    swe_ibvp_fe = @swe_ibvp_fe_muscl_riem;
  elseif strcmp( vars, 'csrv' ) == 1
    swe_ibvp_fe = @swe_ibvp_fe_muscl_csrv;
  elseif strcmp( vars, 'prmv' ) == 1
    swe_ibvp_fe = @swe_ibvp_fe_muscl_prmv;
  else
    disp('The variable option is not defined');
  end

  % Initial condition
  g = 9.8;
  h_l = 1.0; h_r = 0.1;
  u_l = 0.0; u_r = 0.0;
  h = h_l * (x<0) + h_r * (x>0); % h is 1 X N
  u = u_l * (x<0) + u_r * (x>0); % u is 1 X N
  U = [h; h.*u]; % Conservative variable array: 2 X N

  % Solving the shallow water equation
  finish = 0;
  % Computing the time step size
  k = cfl * dx / max( abs(u) + sqrt(g*h) );

  while finish == 0
    if k > T % Check if the remaining time is smaller than k
      k = T;
      finish = 1;
    end
    U_stage1 = swe_ibvp_fe( N, k, dx, h_l, u_l, h_r, u_r, g, nslope, U );
    U_stage2 = swe_ibvp_fe( N, k, dx, h_l, u_l, h_r, u_r, g, nslope, U_stage1 );
    U_next = 0.5*(U+U_stage2);
    U = U_next;
    %U = 0.5*(U + U_stage2);
    h = U(1,:);
    u = U(2,:)./U(1,:);
    % Compute the next time step size
    k = cfl * dx / max( abs(u) + sqrt(g*h) );
    T = T - k; % Remove time step size from remaining time.
  end
end
