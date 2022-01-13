function [x, h, u] = swe_riemann_ref(N)
  % Reference solution to the Riemann problem of
  % shallow water equation with left data:
  %  h_l = 1.0, u_l = 0.0
  % and right data:
  %  h_r = 0.1, u_r = 0.0
  % The solution is composed of a left rarefaction
  % and a right shock:
  % h_l, u_l <- rarefaction -> h_m, u_m 
  % <- shock -> h_r, u_r

  % Basic information
  g = 9.8;
  h_l = 1.0; u_l = 0.0; h_r = 0.1; u_r = 0.0;
  cs_l = sqrt(g*h_l); cs_r = sqrt(g*h_r);
  T = 0.2;
  x = linspace(-1,1,N+1);
  % Intermediate state, details omitted.
  H_M = @(h)2*(sqrt(h_l)-sqrt(h)).*sqrt(h_r).*sqrt(h)-sqrt(0.5*(h_r.^2-h.^2).*(h_r-h));
  h_m = fzero(H_M,0.5*(h_l+h_r)); cs_m = sqrt(g*h_m);
  u_m = u_l+2*cs_l-2*cs_m;
  % Shock speed:
  %               x=st
  % h_m, u_m      /      h_r, u_r
  s = (h_r*u_r-h_m*u_m)/(h_r-h_m);
  % Rarefaction:
  %        x=s_l t     x=s_m t
  % h_l, u_l   \\\\\||||||    h_m, u_m
  s_m = u_m - cs_m;
  s_l = u_l - cs_l;

  % Computing the reference solution
  % All but rarefaction
  h = (x>=s*T)*h_r + (x<s*T).*(x>=s_m*T)*h_m + (x<=s_l*T)*h_l;
  u = (x>=s*T)*u_r + (x<s*T).*(x>=s_m*T)*u_m + (x<=s_l*T)*u_l;
  % Rarefaction:
  ix = find( x>s_l*T & x<s_m*T );
  for j = 1:length(ix)
    x_loc = x(ix(j));
    s_loc = x_loc/T; % this is u-cs, u+2cs = u_l+2cs_l
    u_loc = (2*s_loc+u_l+2*cs_l)/3;
    cs_loc = (u_l+2*cs_l-s_loc)/3;
    h_loc = cs_loc*cs_loc/g;
    u(ix(j)) = u_loc;
    h(ix(j)) = h_loc;
  end
end
