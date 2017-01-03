function RangeVectors = SimpSatSim(t,Lat,Lon,Alt)
%
% Simplified GPS satellite simulation, assuming a spherical earth and
% something like the GPS configuration of 08 March, 2006 (29 satellites)
% as downloaded from www.navcen.uscg.gov - /ftp/GPS/almanacs/yuma/
%
% INPUTS
%   t    time [sec] from when the vernal equinox was at the prime meridian
%   Lat  latitude of the antenna in radians
%   Lon  longitude of the antenna radians 
%   Alt  altitude of the antenna in meters
%
% OUTPUTS
%   RangeVectors  a 3x24 matrix, the columns of which are the unit vectors
%                 from the GPS antenna to the 24 GPS satellites, scaled
%                 by the pseudoranges.   That is, the pseudoranges are the
%                 magnitudes of the column vectors, and the column vectors
%                 divided by the pseudoranges are the unit vectors toward
%                 the satellites from the antenna.
%
Rearth        = ?; % radius of earth
Rorbit        = ?; % radius of satellite orbits
%
% 1. Calculate antenna position in ECI coordinates
%
EarthRotAngle = t*2*pi/24/60/60 % assumes 24-hr siderial day
ECILon        = EarthRotAngle + Lon; % ECI longitude [rad]
slonant       = sin(ECILon);
clonant       = cos(ECILon);
slatant       = sin(Lat);
clatant       = cos(Lat);
ECIantenna    = (Rearth+Alt)*[clonant*clatant;slonant*clatant;slatant];
%
SatRotAngle   = t*2*pi/12/60/60 % assumes 12-hr orbit period
%
for n=1:24,  % satellite number