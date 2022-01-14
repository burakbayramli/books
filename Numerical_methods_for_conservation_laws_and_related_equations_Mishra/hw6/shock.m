function [ u0 ] = shock()
  u0 = @(x) x < 0;
end
