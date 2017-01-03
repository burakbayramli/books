function [posECEF, posVelECI] = getSatelliteState2(almanac, dateStructure, timeVector)

mu = 3.986005e14;                   % m3/sec2
OMEGAdot_earth = 7.2921151467e-5;   % rad/sec

wait_h = waitbar(0, 'Computing satellite state...');

num_PRNs = size(almanac,2);

Theta_GST = GreenwichSidereal(dateStructure, timeVector);

posECEF = zeros(length(timeVector)*num_PRNs, 5);
posVelECI = zeros(length(timeVector)*num_PRNs, 8);

k=1

n = sqrt(mu/almanac.a^3);
t = timeVector - almanac.toa;
M_new = almanac.M + n.*t; 

M_new = mod(M_new,2*pi);

Eold = M_new;
correction = ones(size(timeVector));
tolerance = 10e-12;
while (sum(correction > tolerance) > 0)
    Enew = Eold - (Eold - M_new - almanac.e.*sin(Eold))./(1-almanac.e.*cos(Eold));
    correction = abs(Enew - Eold);
    Eold = Enew;
end
E = Eold;

nu = atan2( ((sqrt(1-almanac.e^2).*sin(E))./(1-almanac.e.*cos(E))),...
    (cos(E)-almanac.e)./(1-almanac.e.*cos(E)) );
nu = mod(nu, 2*pi);

PHI = nu + almanac.omega;  % argument of latitude
OMEGA = mod(almanac.OMEGAo + (almanac.OMEGAdot - OMEGAdot_earth) .* t...
    - OMEGAdot_earth * almanac.toa,2*pi);

p = almanac.a*(1-almanac.e^2);
r = p./(1+almanac.e.*cos(nu));
h = sqrt(mu*p);

x_prime = r.*cos(PHI);
y_prime = r.*sin(PHI);

OMEGA_ECI = OMEGA + Theta_GST;

x_ECEF = (x_prime.*cos(OMEGA) - y_prime.*cos(almanac.i).*sin(OMEGA)); 
y_ECEF = (x_prime.*sin(OMEGA) + y_prime.*cos(almanac.i).*cos(OMEGA));
z_ECEF = (y_prime.*sin(almanac.i));

x_ECI = (x_prime.*cos(OMEGA_ECI) - y_prime.*cos(almanac.i).*sin(OMEGA_ECI)); 
y_ECI = (x_prime.*sin(OMEGA_ECI) + y_prime.*cos(almanac.i).*cos(OMEGA_ECI));
z_ECI = (y_prime.*sin(almanac.i));

xdot = -mu/h .* (cos(OMEGA) .* (sin(PHI) + almanac.e*sin(almanac.omega)) +...
    sin(OMEGA) .* cos(almanac.i) .* (cos(PHI) + almanac.e*cos(almanac.omega)));
ydot = -mu/h .* (sin(OMEGA) .* (sin(PHI) + almanac.e*sin(almanac.omega)) -...
    cos(OMEGA) .* cos(almanac.i) .* (cos(PHI) + almanac.e*cos(almanac.omega)));
zdot = mu/h * sin(almanac.i) .* (cos(PHI) + almanac.e*cos(almanac.omega));

posECEF(length(timeVector)*(k-1)+1:length(timeVector)*k, :) = [timeVector almanac.prn*ones(size(timeVector)) x_ECEF y_ECEF z_ECEF];
posVelECI(length(timeVector)*(k-1)+1:length(timeVector)*k, :) = [timeVector almanac.prn*ones(size(timeVector)) x_ECI y_ECI z_ECI xdot ydot zdot];

update_str = sprintf('Computing satellite state... (%i%%)', floor(k/num_PRNs*100));
waitbar(k/num_PRNs, wait_h, update_str);

close(wait_h);
