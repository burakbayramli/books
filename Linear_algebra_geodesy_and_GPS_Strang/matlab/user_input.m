% Location

lat = 40;     % latitude in deg (North - positive; South - negative)
lon = -105;   % longitude in deg (East - positive; West - negative)
alt = 1631;   % height above the WGS 84 ellipsoid in meters

% Date / Time / Duration
year = 2007;             
month = 9;      % 1 - 12
day = 7;        % 1 - 31
hour = 0;       % hours in military time, 0 - 23
minute = 0;     % 0 - 59
second = 0;     % 0 - 59

duration = [ 24 0 ];   % [ hours minutes ]
dt = 60;             % time step in seconds
PRN = [ 0 ];         % satellite PRNs used; enter 0 to calculate for all PRN's

% Elevation Mask

% Notes:
% 0 <= AZ <= 360 deg
% -90 <= EL <= 90 deg
% The first azimuth element, e.g. 10, covers azimuths from 0 - 10 degrees.
% The second azimuth element, e.g. 20, covers azimuths > 10 and <= 20
% degrees.  Therefore, the cut-off elevation corresponding to the first
% azimuth element is constant for that range of azimuths.

mask.az = [360];
mask.el=  [0];


