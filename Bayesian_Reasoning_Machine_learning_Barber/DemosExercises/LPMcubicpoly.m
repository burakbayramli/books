function phi = LPMcubicpoly(x)
phi(1,:) = ones(1,length(x)); % cubic polynomial :
phi(2,:) = x;
phi(3,:) = x.^2;
phi(4,:) = x.^3;