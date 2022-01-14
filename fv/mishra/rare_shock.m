function [ u0 ] = rare_shock()
  u0 = @(x) (x > 0) .* (x < 1);
end
