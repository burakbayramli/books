function [f,rho,u,v] = zero_out_of_bounds(f,rho,u,v,lasts)
% lasts: lasts(j) is the last i position that should be excluded from the 
%   simulation (up to i).

for k = 1:length(lasts)
    f(k,1:lasts(k), :) = 0;
    rho(k,1:lasts(k)) = 0;
    u(k,1:lasts(k)) = 0;
    v(k,1:lasts(k)) = 0;
end