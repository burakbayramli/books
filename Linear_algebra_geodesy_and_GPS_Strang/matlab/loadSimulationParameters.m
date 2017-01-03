%*******************************************************
% function simulation = loadSimulationParameters(filename)
%
% DESCRIPTION:
%  This function compiles all needed information to simulate the orbits of
%  the GPS satellites.  The information is organized into a struct for ease
%  of data passing.
%  
% ARGUMENTS:
%  filename - file of user inputs for location, date, and simulation (see
%  user_input.m for example of inputs)
%   *can also call skyplotGUI.m for user-friendly input
%  
% OUTPUT:
%  simulation - struct of all pertinent information for simulation
%       .location - struct of user location (LLA and xyz)
%       .date - struct of date information (YMDhms)
%       .mask - struct of elevation mask information
%       .time - vector of time values at which to perform calculations
%       .PRN - vector list of PRNs to simulate
%       .almanac - struct of almanac values from YUMA file
%  
% CALLED BY:
%  GPSVisibilityTool
%
% FUNCTIONS CALLED:
%  loadInput
%  getYUMAFileName
%  readYUMAfile
%  trimAlmanac
%*******************************************************

function simulation = loadSimulationParameters(filename)


% Load input and simulation
[location, dateStructure, mask, duration, dt, PRN] = loadInput(filename);

% Compute vector of time values
stop_time = dateStructure.TOW + duration(1) * 3600 + duration(2) * 60;
timeVector = [dateStructure.TOW:dt:stop_time]';

% Load almanac data
almanac = getYumaAlmanac(dateStructure);

% Trim PRN list to pertinent values
[PRN, almanac] = trimAlmanac(PRN, almanac);
if isempty(PRN)
    errordlg('Almanac data not available for selected PRNs', 'Error');
    error('Almanac data not available for selected PRNs');
end

% Populate simulation struct
simulation.location = location;
simulation.dateStructure = dateStructure;
simulation.mask = mask;
simulation.timeVector = timeVector;
simulation.PRN = PRN;
simulation.almanac = almanac;
