
%*******************************************************
% function [ vis ] = run_vis(input_file)
%
% DESCRIPTION:
%     Top-level function to run GPS visibility codes
%     using YUMA almanacs.
%  
% ARGUMENTS:
%     input_file (str) - name of input file, usually 'user_input.m'
%  
% OUTPUT:
%     vis - visibility structure containing
% 
%           vis.data - visibility data matrix
%           vis.col - column definitions for vis.data
%
% EXAMPLE:
%     vis = run_vis('user_input');
%
% FUNCTIONS CALLED:
%     loadSimulationParameters.m
%     getSatelliteState.m
%     getAzEl.m
%
%*******************************************************

function [ vis ] = run_vis(input_file)

% Load simulation parameters
simulation = loadSimulationParameters(input_file);

% Compute SV position and velocity
[ satPosECEF, satPosVelECI ] = getSatelliteState(simulation.almanac, ...
    simulation.dateStructure, simulation.timeVector);

% Test output
% simulation.location
% satPosECEF(1:5, :)


% STUDENTS MUST WRITE get_AzEl_5090.m
% Compute SV az/el 
satAzEl = getAzEl_5090(simulation.location, satPosECEF);


% Populate vis struct
azel_col = 3:4;
vis.data = [ satPosVelECI satAzEl(:, azel_col) ];
vis.col.TOW = 1;
vis.col.PRN = 2;
vis.col.X = 3;
vis.col.Y = 4;
vis.col.Z = 5;
vis.col.X_DOT = 6;
vis.col.Y_DOT = 7;
vis.col.Z_DOT = 8;
vis.col.AZ = 9;
vis.col.EL = 10;
