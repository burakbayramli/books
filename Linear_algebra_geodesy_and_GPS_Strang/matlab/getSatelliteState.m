%*******************************************************
% function [posECEF, posVelECI] = getSatelliteState(almanac, date, time)
%
% DESCRIPTION:
%  compute ECI satellite position and velocity when given Keplerian
%  orbital elements from almanac file.
%  Assumes all angles are in radians, and all distances are in meters.
%  
% ARGUMENTS:
%  almanac - struct containing almanac information (see readYUMAFile.m)
%  date - date struct (see loadInput.m) for calculating Greenwich Sidereal
%       time
%  time - vector of time values
%  
% OUTPUT:
%       posECEF - matrix of ECEF position information for each PRN at
%           each time step.  Formatted as: [time  PRN  x  y  z]
%       posVelECI - matrix of ECI position and velocity information for each PRN at
%           each time step.  Formatted as: [time  PRN  x  y  z  xdot  ydot  zdot]
%  
% CALLED BY:
%       GPSVisibilityTool
%
% FUNCTIONS CALLED:
%       GreenwichSidereal
%*******************************************************
function [posECEF, posVelECI] = getSatelliteState(almanac, dateStructure, timeVector)

mu = 3.986005e14;                   % m3/sec2
OMEGAdot_earth = 7.2921151467e-5;   % rad/sec

wait_h = waitbar(0, 'Computing satellite state...');

num_PRNs = size(almanac,2);

% Calculate Greenwich Sidereal Time at each time step for ECI conversions
Theta_GST = GreenwichSidereal(dateStructure, timeVector);

% Initialize matrices
posECEF = zeros(length(timeVector)*num_PRNs, 5);
posVelECI = zeros(length(timeVector)*num_PRNs, 8);

% Propogate satellite orbits
for k = 1:num_PRNs  % loops through PRNs
    n = sqrt(mu/almanac(k).a^3);
    t = timeVector - almanac(k).toa;
    M_new = almanac(k).M + n.*t; 
    
    % make sure M is in the correct quadrant
    M_new = mod(M_new,2*pi);
    
    Eold = M_new;
    correction = ones(size(timeVector));
    tolerance = 10e-12;
    while (sum(correction > tolerance) > 0)
        % Solve for the eccentric anomaly using Newton's iteration scheme
        Enew = Eold - (Eold - M_new - almanac(k).e.*sin(Eold))./(1-almanac(k).e.*cos(Eold));
        correction = abs(Enew - Eold);
        Eold = Enew;
    end
    E = Eold;
    
    % find true anomaly 
    nu = atan2( ((sqrt(1-almanac(k).e^2).*sin(E))./(1-almanac(k).e.*cos(E))),...
        (cos(E)-almanac(k).e)./(1-almanac(k).e.*cos(E)) );
    nu = mod(nu, 2*pi);
    
    PHI = nu + almanac(k).omega;  % argument of latitude
    OMEGA = mod(almanac(k).OMEGAo + (almanac(k).OMEGAdot - OMEGAdot_earth) .* t...
        - OMEGAdot_earth * almanac(k).toa,2*pi);
    
    p = almanac(k).a*(1-almanac(k).e^2);
    r = p./(1+almanac(k).e.*cos(nu));
    h = sqrt(mu*p);
    
    % position in the orbital plane:
    x_prime = r.*cos(PHI);
    y_prime = r.*sin(PHI);
    
    OMEGA_ECI = OMEGA + Theta_GST;
    
    % satellite position in ECEF coordinates (in meters)
    x_ECEF = (x_prime.*cos(OMEGA) - y_prime.*cos(almanac(k).i).*sin(OMEGA)); 
    y_ECEF = (x_prime.*sin(OMEGA) + y_prime.*cos(almanac(k).i).*cos(OMEGA));
    z_ECEF = (y_prime.*sin(almanac(k).i));
    
    % satellite position in ECI coordinates (in meters)
    x_ECI = (x_prime.*cos(OMEGA_ECI) - y_prime.*cos(almanac(k).i).*sin(OMEGA_ECI)); 
    y_ECI = (x_prime.*sin(OMEGA_ECI) + y_prime.*cos(almanac(k).i).*cos(OMEGA_ECI));
    z_ECI = (y_prime.*sin(almanac(k).i));

    % satellite velocity in ECI, from Prussing & Conway, pg. 54
    xdot = -mu/h .* (cos(OMEGA) .* (sin(PHI) + almanac(k).e*sin(almanac(k).omega)) +...
        sin(OMEGA) .* cos(almanac(k).i) .* (cos(PHI) + almanac(k).e*cos(almanac(k).omega)));
    ydot = -mu/h .* (sin(OMEGA) .* (sin(PHI) + almanac(k).e*sin(almanac(k).omega)) -...
        cos(OMEGA) .* cos(almanac(k).i) .* (cos(PHI) + almanac(k).e*cos(almanac(k).omega)));
    zdot = mu/h * sin(almanac(k).i) .* (cos(PHI) + almanac(k).e*cos(almanac(k).omega));
  
    posECEF(length(timeVector)*(k-1)+1:length(timeVector)*k, :) = [timeVector almanac(k).prn*ones(size(timeVector)) x_ECEF y_ECEF z_ECEF];
    posVelECI(length(timeVector)*(k-1)+1:length(timeVector)*k, :) = [timeVector almanac(k).prn*ones(size(timeVector)) x_ECI y_ECI z_ECI xdot ydot zdot];
    
    update_str = sprintf('Computing satellite state... (%i%%)', floor(k/num_PRNs*100));
    waitbar(k/num_PRNs, wait_h, update_str);
end % next PRN

close(wait_h);