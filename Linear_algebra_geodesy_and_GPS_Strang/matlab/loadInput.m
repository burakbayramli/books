%*******************************************************
% function [location, date, mask, duration, dt, PRN] = loadInput(filename)
%
% DESCRIPTION:
%  This function loads information on the user location, the date of the
%  simulation, the elevation mask, the duration and time step, and which
%  PRNs to evaluate and checks for acceptable values.
%  Data are stored in structs containing various pertinent values.
%  
% ARGUMENTS:
%  filename - file of user input
%  
% OUTPUT:
%  location - struct containing latitude, longitude, altitude and
%       xyz position in ECEF
%  date - stuct containing calendar date (YMDHMS) and the GPS week
%       and Time of Week (TOW)
%  mask - struct specifying the elevation mask
%  duration - vector specifying simulation duration [hh mm]
%  dt - time step for simulation
%  PRN - vector of PRNs to simulate
%  
% CALLED BY:
%  getConstellationData
%
% FUNCTIONS CALLED:
%  wgslla2xyz
%  wgsxyz2lla
%  GPSweek
%*******************************************************

function [location, dateStructure, mask, duration, dt, PRN] = loadInput(filename)


% replaces the above commented code
loadFileVariablesIntoWS(filename);

% Date struct
dateStructure.year = year;
dateStructure.month = month;
dateStructure.day = day;
dateStructure.hour = hour;
dateStructure.min = minute;
dateStructure.sec = second;

% Location struct
if exist('lat')
    location.lat = lat;
    location.lon = lon;
    location.alt = alt;

    xyz = wgslla2xyz([ location.lat ; location.lon ; location.alt ]);
    location.x = xyz(1);
    location.y = xyz(2);
    location.z = xyz(3);
    
else
    location.x = x;
    location.y = y;
    location.z = z;
    xyz = [location.x; location.y; location.z];
    
    LLA = wgsxyz2lla(xyz);
    
    location.lat = LLA(1);
    location.lon = LLA(2);
    location.alt = LLA(3);
end


[dateStructure.GPSwk, dateStructure.TOW] = ...
    GPSweek(dateStructure.year, dateStructure.month, ...
    dateStructure.day, dateStructure.hour, dateStructure.min, ...
    dateStructure.sec);

% Mask table struct loaded straight from file
if mask.az(end) ~= 360
    mask.az(end+1) = 360;
    mask.el(end+1) = 0;
end
  


% Errors for invalid inputs
if location.lat < -90 | location.lat > 90
    displayError('Error: Reference Latitude out of range');
elseif location.lon < -180 | location.lon > 180
    displayError('Error: Reference Longitude out of range');
elseif dateStructure.year < 1000
    displayError('Error: Reference Year must contain 4 digits');
elseif dateStructure.month < 1 | dateStructure.month > 12
    displayError('Error: Invalid entry for Reference Month');
elseif dateStructure.day < 1 | dateStructure.day > 31
    displayError('Error: Invalid entry for Reference Date');
elseif dateStructure.hour < 0 | dateStructure.hour > 23
    displayError('Error: Invalid entry for Reference Hour');
elseif dateStructure.min < 0 | dateStructure.min > 59
    displayError('Error: Invalid entry for Reference Minute');
elseif dateStructure.sec < 0 | dateStructure.sec > 59
    displayError('Error: Invalid entry for Reference Second');
elseif julianDate([year month day hour minute second]) > julianDate(clock)
    displayError('Error: The date enterred occurs in the future');
elseif size(mask.az,2) ~= size(mask.el,2)
    displayError('Error: Mask vectors not the same size');
elseif duration(1) < 0
    displayError('Error: Invalid entry for hours of Duration');
elseif duration(2) < 0
    displayError('Error: Invalid entry for minutes of Duration');
elseif sum(duration) <= 0
    displayError('Error: Invalid input for simulation duration');
elseif dt <= 0
    displayError('Error: Invalid Time Step');
end


% =========================================================================
function displayError(string)
    errordlg(string,'Error');
    error(string)
