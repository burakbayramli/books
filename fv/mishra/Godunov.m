function [ flux ] = Godunov( f, f_prime, f_type, f_extrema )

  flux = @(j, u, dx, dt) flux_eval(j, u, dx, dt, f, f_prime, f_type, f_extrema);
end

function [flux] = flux_eval( j, u,  ~,  ~, f,  ~, f_type, f_extrema )

  switch f_type
    case 'convex',
      flux = max( f(max(u(j), f_extrema)), f(min(u(j+1),f_extrema)) );
    case 'concave',
      flux = min( f(min(u(j), f_extrema)), f(max(u(j+1),f_extrema)) );
    otherwise
      flux = 0;
  end
end


