% Satellite motion simulation and measurement sensitivity matrix calculation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For each time step, this module calculates the equations of motion      %
% advancing the GPS satellite position, then uses the updated position to %
% calculate an updated H matrix.                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
for sat = 1:4
%
% Satellite equations of motion
   Omega = Omega_0(sat) + Omega_rate*t;
   Theta = Theta_0(sat) + Theta_rate*t;
%
% Satellite position vectors and pseudorange
%
   x(sat)   = Rad*(cos(Theta)*sin(Omega) + sin(Theta)*cos(Omega)*CosIncl);
   y(sat)   = Rad*sin(Theta)*SinIncl;
   z(sat)   = Rad*(cos(Theta)*cos(Omega) - sin(Theta)*sin(Omega)*CosIncl) - ERad;
   rho(sat) = sqrt(x(sat)^2 + y(sat)^2 + z(sat)^2);
%
% Assign calculated values to H matrix elements
%
   H(sat,1) =  - x(sat)/rho(sat);
   H(sat,2) =  - y(sat)/rho(sat);
   H(sat,3) =  - z(sat)/rho(sat);
   
end;