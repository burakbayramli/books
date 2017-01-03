function CECEF2ENU = ECEF2ENU(LatitudeInRadians,LongitudeInRadians),
%
% Computes the coordinate transformation matrix from 
% Earth-Centered_Earth_Fixed (ECEF) coordinates to
% local East-North-Up coordinates.
%
% INPUTS
%
%   LatitudeInRadians
%   LongitudeInRadians
%
% OUTPUT
%
%   CECEF2ENU, 3x3 coordinate transformation matrix from ECEF coordinates
%   to ENU coordinates.
%
sLon = sin(LongitudeInRadians);
cLon = cos(LongitudeInRadians);
sLat = sin(LatitudeInRadians);
cLat = cos(LatitudeInRadians);
CECEF2ENU = [-sLon,cLon,0;-cLon*sLat,-sLon*sLat,cLat;cLon*cLat,sLon*cLat,sLat];
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  
