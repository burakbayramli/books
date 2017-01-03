% Initialize GPS satellites
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GPS satellites ae assumed to be in a circular oribital trajectory  %
% at a 55 deg inclination angle.  It is assumed that the satellites  %
% orbit the earth at a constant rate 'Theta' with a period of        %
% 43082 seconds.                                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
dtr     = pi/180;								% Conversion from degrees to radians
rtd     = 180/pi;								% Conversion from radians to degrees
Rad     = 26560000;							% Radius of satellite orbit [meter]
ERad    = 6380000;							% Earth radius [meter]
Incl    = 55;									% Angle of Inclination [deg]
CosIncl = cos(Incl*dtr);					% Cosine of Inclination angle
SinIncl = sin(Incl*dtr);					% Sine of Inclination angle
%
Omega_0_all = dtr*[326, 26, 146, 86, 206];
Theta_0_all = dtr*[68, 340, 198, 271, 90];
if (default=='y')|(default=='Y')
   Omega_0  = Omega_0_all(1:4);			% Right ascension [deg]
	Theta_0  = Theta_0_all(1:4);			% Angular location of satellite in orbit [deg]
else
   Omega_0  = Omega_0_all(sat_set);		% Right ascension [deg]
	Theta_0  = Theta_0_all(sat_set);		% Ang loc of satellite in orbit [deg]
end
Omega_rate  = 2*pi/86164;					% Angular rate of ascension [rad/sec] (1 rev/day)
Theta_rate  = 2*pi/43082;					% Angular rate of sat loc in orbit [rad/sec] (2 rev/day)
%
Pplus_last  = P0_plus;						% Initialize for Kalman filter
