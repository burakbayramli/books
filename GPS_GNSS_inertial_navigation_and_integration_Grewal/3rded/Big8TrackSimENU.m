function [xENU,vENU,aENU,aSensedENU,omegaENU,omegaSensedENU] = Big8TrackSimENU(t,LatRad,LonRad,Alt)
%
% Simulated dynamic conditions on a 100 km figure-8 track 
%
% Track length = 10^4/6 meters = 10/6 km
% Crossover bridge height      = 10 meters
% Lap time                     = 60 seconds
%
% INPUT
%
%   t = time from undercrossing [s]
%
% OUTPUTS (all column vectors representing INS state in SI un   its)
%
%   xENU            = [Easting;Northing;Altitude] with respect to undercrossing location
%   vENU            = [vE;vN;vU] velocity with respect to track
%   aSensedENU      = [aE;aN;aU] sensed acceleration with respect to track
%   omegaSensedENU  = sensed rotation rates, including earthrate
%                       [rad/s]
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  
%
% PARAMETER SETTINGS
%
S            = 6458.1337842;      % [m] scaling for 10/6 km track length
Tlap         = 3600;              % [s] lap time (one hour)
BridgeHeight = 10;                % [m]
MaxBank      = 0.00779609488045;  % [rad] ~0.44668 [deg]
%LatDeg       = 39.7931;          % Approximate location of 
%LonDeg       = -86.2389;         % Indianapolis Motor Speedway
%Altitude     = 221;              %
%LatRad       = LatDeg*pi/180;
%LonRad       = LonDeg*pi/180;
sLat         = sin(LatRad);
cLat         = cos(LatRad);
omegaEarth   = 0.7292115e-4 * [0;cLat;sLat]; % Earthrate in ENU coordinates
%
s = sin(0.2e1*pi*t/Tlap);
c = cos(0.2e1*pi*t/Tlap);
%
% Cartesian variables
%
xENU        = [0.2e1*S*s*c;0.3e1*S*s;BridgeHeight*(0.1e1 - c)/0.2e1];
vENU        = [0.4e1*S*pi*(c - s)*(c + s)/Tlap;0.6e1*S*c*pi/Tlap;BridgeHeight*s*pi/Tlap];
aENU        = [-0.32e2*S*pi^2*c*s/Tlap^2;-0.12e2*S*s*pi^2/Tlap^2;0.2e1*BridgeHeight*c*pi^2/Tlap^2];
speed       = norm(vENU);
speedsq     = speed^2;
omegaENU    = cross(vENU,aENU)/speedsq;
aSensedENU  = aENU + [0;0;9.8];
omegaSensedENU = omegaENU + omegaEarth;