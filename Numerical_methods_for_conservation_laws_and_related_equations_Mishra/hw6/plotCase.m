function [] = plotCase(ic_str,N,plot_res,axis_limits,T,l,r,f,f_prime,f_type,f_extrema,u0,u_e)
  figure ;
  [ x, u, dx ] = solve_all( f, f_prime, f_type, f_extrema, u0,l, r, T, plot_res );

  %subplot (2,2,[1 2]);
  plot (x, u, '-o', x, feval (getCellAverages(u_e, dx), x));
  title (strcat('Solution for', ic_str, ' initial conditions'));
  xlabel ('x');
  ylabel ('u(x,1)');
  if strcmp(ic_str, ' rarefaction')
    axis ([l r -1.5 1.5]);
  else
    axis ([l r -0.5 1.5]);
  end
  legend ('Godunov','Exact solution');  
