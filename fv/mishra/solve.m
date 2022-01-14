function [ x, u, dx ] = solve( f_prime, flux, u0, l, r, T, mesh_size )
  [ x, dx ] = getMesh( r, l, mesh_size );
  u = feval (getCellAverages(u0, dx), x);

  t = 0;
  while (t < T)
    dt = 1/2 * dx / max(abs(f_prime(u)));
    t = min(t + dt, T);
    v = u;
    v = apply_outflow_bc(v);
    for j = 2:length (x)-1
      u(j) = v(j) - dt/dx * (flux(j, v, dx, dt) - flux(j-1,v, dx, dt));
    end

  end

  u = u(2:end-1);
  x = x(2:end-1);

end
