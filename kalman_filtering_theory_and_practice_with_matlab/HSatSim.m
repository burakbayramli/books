function HGPS = HSatSim(t,Lat,Lon,Alt) 
% 
% Grewal & Andrews, Kalman Filtering: Theory and Practice Using MATLAB, 
% John Wiley & Sons, 2008 
% 
% 
% Simplified GPS satellite simulation for measurement sensitivity matrix 
% H in locally level North-East-Down coordinates. 
% 
% Simplified GPS satellite model assumes a spherical earth and 
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
%   HGPS  a 29x3 matrix, the columns of which are the unit vectors 
%         from the GPS antenna to the 29 GPS satellites, in locally 
%         level coordinates 
% 
global RA PA % right ascensions and initial orbit angles of satellites; 
             % requires call to YUMAdata to initialize

Rearth        = 20000e3/pi; % radius of earth, based on original definition 
                            % of the meter (10,000 km from pole to equator) 
                            % (around 6366197.723675814 m) 
Rorbit        = 26560000;   % radius of satellite orbits [m] 
s55           = 0.81915204428899; % sine of 55-deg inclination angle 
c55           = 0.57357643635105; % cosine of 55-deg inclination angle 
% 
% Calculate locally level axis directions (North East Down) 
%                         at antenna position 
%                         in ECI coordinates 
% 
EarthRotAngle = t*2*pi/24/60/60; % assumes 24-hr siderial day 
ECILon        = EarthRotAngle + Lon; % ECI longitude [rad] 
slonant       = sin(ECILon); 
clonant       = cos(ECILon); 
slatant       = sin(Lat); 
clatant       = cos(Lat); 
ECIeast       = [-slonant;clonant;0]; 
ECIdown       = -[clonant*clatant;slonant*clatant;slatant]; 
ECInorth      = [-clonant*slatant;-slonant*slatant;clatant]; 
% 
% Calculate ECI-to-NED coordinate transformation matrix 
% 
ECI2NED       = [ECInorth,ECIeast,ECIdown]'; 
% 
% Calculate antenna position in ECI coordinates 
% 
ECIantenna    = -(Rearth+Alt)*ECIdown; 
% 
% Calculate transformation from ECI to locally level NED coordinates 
% 
% 
% Calculate satellite locations relative to antenna in ECI coordinates  
% 
HGPS          = []; 
SatRotAngle   = t*2*pi/12/60/60; % assumes 12-hr orbit period 
% 
NumSats       = length(RA); 
% 
for n=1:NumSats,  % n is satellite order, not PR number 
    SatAngle = PA(n) + SatRotAngle; % satellite angle from ascending node 
    sSA      = sin(SatAngle); 
    cSA      = cos(SatAngle); 
    SLat     = sSA*s55;   % sine of satellite latitude in ECI c. 
    CLat     = sqrt(1 - SLat^2);    % cosine of satellite latitude 
    cdLon    = cSA/CLat; 
    sdLon    = (cdLon*cSA-CLat)/sSA/c55; 
    dLon     = atan2(sdLon,cdLon); 
    SatLon   = RA(n)+dLon; 
    sSLon    = sin(SatLon); 
    cSLon    = cos(SatLon); 
    ECIsat   = Rorbit*[cSLon*CLat;sSLon*CLat;SLat]; 
    Vant2sat = ECIsat - ECIantenna; 
    Uant2sat = Vant2sat/norm(Vant2sat); 
    HGPS     = [HGPS;-Uant2sat']; 
end;
