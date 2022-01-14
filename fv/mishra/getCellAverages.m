function [ u_avg ] = getCellAverages( u, dx )
  u_avg = @(x) avg_eval(x, u, dx);
end

function u_avg = avg_eval(x, u, dx)
  u_avg = zeros (1, length (x));
  for i=1: length (x)
    u_avg(i) = quad(u, x(i) - dx/2, x(i) + dx/2)/dx;
  end

end

