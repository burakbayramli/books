function U_next = swe_ibvp_fe_muscl_prmv( N, k, dx, h_l, u_l, h_r, u_r, g, nslope, U )
  h_slope = zeros(1,N);
  u_slope = zeros(1,N);
  F = zeros(2,N+1);
  U_next = zeros(2,N);
  h = U(1,:);
  u = U(2,:)./U(1,:);

  % Compute the slopes - skipe the left most one and the right most one
  for i = 2:N-1
    h_slope(i) = nslope(dx,h(i-1),h(i),h(i+1)); % Slope for h
    u_slope(i) = nslope(dx,u(i-1),u(i),u(i+1)); % Slope for u
  end
  % Compute the fluxes
  F(:,1) = [h_l*u_l; h_l*u_l*u_l+0.5*g*h_l*h_l]; % Left boundary
  for i = 2:N
    h_left = h(i-1)+0.5*dx*h_slope(i-1);
    u_left = u(i-1)+0.5*dx*u_slope(i-1);
    h_rght = h(i)-0.5*dx*h_slope(i);
    u_rght = u(i)-0.5*dx*u_slope(i);
    U_left = [h_left; h_left*u_left];
    U_rght = [h_rght; h_rght*u_rght];
    F(:,i) = swe_roe_flux(g,U_left,U_rght);
  end
  F(:,1+N) = [h_r*u_r; h_r*u_r*u_r+0.5*g*h_r*h_r]; % Right boundary
  % Update the solution using FE time integrator
  for i = 1:N
    U_next(:,i) = U(:,i) - (k/dx) * (F(:,i+1)-F(:,i));
  end
end
