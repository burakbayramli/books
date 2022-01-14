function [ x, u, dx] = solve_all( f, f_prime, f_type,f_extrema, u0, l, r, T, mesh_size )
  % Godunov scheme
  flux = Godunov( f, f_prime, f_type, f_extrema);
  [x, u(1,:), dx] = solve(f_prime, flux, u0, l, r, T,mesh_size);
  [~, u(2,:),~] = solve(f_prime, flux, u0, l, r, T, mesh_size);
