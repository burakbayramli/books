function HGNSS = HSatENU(t,LatRad,LonRad,Alt)
%
% Simplified GNSS satellite simulation for pseudorange measurement
% sensitivity matrix (H) to antenna location, in locally-level
% East-North-Up coordinates.
%
% Simplified GPS satellite model assumes a spherical earth and
% the GPS ephemerides of 07 July, 2012, with 31 satellites, last
% downloaded from http://www.navcen.uscg.gov/?pageName=gpsAlmanacs.
%
% These emphmerides were for a configuration with 31 satellites, but the
% program deterines the number of satellites from the stored data.
%
% Users can download and incorporate more current ephemerides by following 
% the instructions given in the m-file FetchYUMAdata.m
%
% INPUTS
%   t       time [s] from when the prime meridian was at the vernal equinox 
%   LatRad  latitude of the receiver antenna in radians
%   LonRad  longitude of the receiver antenna radians 
%   Alt     altitude of the receiver antenna in meters
%
% OUTPUTS (NS = Number of Satellites)
%   HGPS  a NSx3 matrix, the rows of which are the unit vectors
%         from the respective satellite antennas to the GPS antenna,
%         in locally-level ENU coordinates.
%
% FUNCTIONS CALLED
%
%   ECI2ECEF returns transformation matrix from ECI to ECEF coordinates
%   ECEF2ENU returns transformation matrix from ECEF to ENU coordinates
%   
%   
global RA PA % right ascensions and initial orbit plane angles of sats.
             % (must be loaded by caling program)
if length(RA) < 1, error('No satellite data for HSatSim.'); end;
Rearth        = 6371009;    % mean radius of earth [m]
Rorbit        = 26560000;   % radius of satellite orbits [m]
s55           = 0.81915204428899; % sine of 55-deg inclination angle
c55           = 0.57357643635105; % cosine of 55-deg inclination angle
%
% Calculate locally level axis directions (East-North-Up)
%                         at antenna position
%                         in ECI coordinates
%
CECI2ECEF = ECI2ECEF(t);
CECEF2ENU = ECEF2ENU(LatRad,LonRad);
CECI2ENU  = CECEF2ENU*CECI2ECEF;
CENU2ECI  = CECI2ENU';
%
% Calculate antenna position in ECI coordinates
%
ECIantenna    = (Rearth+Alt)*CENU2ECI(:,3);
%
% Calculate satellite locations relative to antenna in ECI coordinates 
% Calculate vector from antenna to satellite in ECI coordinates, normalize
% it, transform to ENU coordinates,, and change sign to form H
%
SatRotAngle   = t*2*pi/12/60/60; % assumes 12-hr orbit period
%
NS            = length(RA);
%
HGNSS         = [];
%
for n=1:NS,  % n is satellite order, not PR number
    SatAngle = PA(n) + SatRotAngle; % satellite angle from ascending node
    sSA      = sin(SatAngle);
    cSA      = cos(SatAngle);
    SLat     = sSA*s55;   % sine of satellite latitude in ECI coordinates
    CLat     = sqrt(1 - SLat^2); % cosine of satellite latitude " "
    cdLon    = cSA/CLat;
    sdLon    = (cdLon*cSA-CLat)/sSA/c55;
    dLon     = atan2(sdLon,cdLon);
    SatLon   = RA(n)+dLon;  % satellite longitude in ECI coordinates
    sSLon    = sin(SatLon);
    cSLon    = cos(SatLon);
    ECIsat   = Rorbit*[cSLon*CLat;sSLon*CLat;SLat]; % sat. vector in ECI
    Vsat2ant = ECIantenna - ECIsat;  % vector from satellite to antenna
    Usat2ant = CECI2ENU*Vsat2ant/norm(Vsat2ant); % in ENU coordinates
    HGNSS    = [HGNSS;Usat2ant'];
end;
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
