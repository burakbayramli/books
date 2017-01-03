function phi = LPMquad(x)
phi =ones(size(x));
phi = vertcat(phi,x);
phi = vertcat(phi,x.^2);
phi = vertcat(phi,x(1,:).*x(2,:));
