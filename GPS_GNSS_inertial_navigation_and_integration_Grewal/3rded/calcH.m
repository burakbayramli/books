%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  
% calcH.m  %% calculates measurement sensitivity matrix for satellites,
% including satellite motion simulation
% 
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