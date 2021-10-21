function pressure = pressure1(data)
% compute the pressure from the data in 1 dimension

gamma = 1.4;
rho = data(:,1);
u = data(:,2)./rho;
energy = data(:,3);
pressure = (gamma-1) * (energy - 0.5*rho.*(u.*u));
