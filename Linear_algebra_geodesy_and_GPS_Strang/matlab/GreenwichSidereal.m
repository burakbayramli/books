%*******************************************************
% function Theta_GST = GreenwichSidereal(date, time)
%
% DESCRIPTION:
%  Calculates the corresponding Greenwich Sidereal Time at each time step
%  in the time vector argument (Reference: Vallado, pg.67)
%  
% ARGUMENTS:
%  date - struct containing date information (see loadInput)
%  time - vector of time values for which to calculate Theta_GST
%  
% OUTPUT:
%  Theta_GST - Greenwich Sidereal Time at each time (rad)
%  
% CALLED BY:
%  getSatelliteState
%
% FUNCTIONS CALLED:
%  julianDate
%
%*******************************************************
function Theta_GST = GreenwichSidereal(dateStructure, timeVector)

sec2day = 1/60/60/24;               % unit conversion

dt = timeVector(2) - timeVector(1);

% calculate the Julian Date from the reference starting time
JD_init = julianDate([dateStructure.year, dateStructure.month, dateStructure.day, dateStructure.hour, dateStructure.min, dateStructure.sec]);

% Calculate the adjusted Julian Date
JD_adj = JD_init*ones(size(timeVector)) + dt*ones(size(timeVector))*sec2day;

% Calculate the number of Julian centuries elapsed from the epoch J2000
T = (JD_adj - 2451545)./36525;

% Update the Greenwich Sidereal Time in radians
Theta_GST = rem((67310.54841 + (876600*3600+8640184.812866).*T + 0.093104.*T.^2 -...
    6.2e-6.*T.^3), 86400)./240 .* pi/180;

% Quadrant check
Theta_GST = mod(Theta_GST,2*pi);
