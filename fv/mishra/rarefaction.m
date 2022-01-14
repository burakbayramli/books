function [ u0 ] = rarefaction()

u0 = @(x) -2*(x < 0) + 1;
end
