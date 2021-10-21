function p = pressure1(data)
% compute the pressure in 1d

gamma = 1.4;
rho = data(:,1);
u = data(:,2)./rho;
speed2 = u.*u;
p = (gamma-1) * (data(:,3) - 0.5*rho.*speed2);
