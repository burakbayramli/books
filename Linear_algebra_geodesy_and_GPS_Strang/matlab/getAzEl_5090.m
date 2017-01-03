%*******************************************************
% function azEl = getAzEl(location, sat_pos)
%
% DESCRIPTION:
%
%  This function takes information about the user location and the satellite
%  positions and determines the azimuth and elevation from the user's
%  vantage point.  The sat_pos matrix is input as ECEF coordinates.
%  
% ARGUMENTS:
%
%  location - struct containing latitude, longitude, altitude and
%       xyz position in ECEF (see loadInput.m)
%  sat_pos - matrix of satellite positions at each time:
%       [time  PRN  x  y  z]
%  
% OUTPUT:
%
%  azEl - matrix of satellite azimuth and elevation:
%       [time  PRN  az  el]
%  
% FUNCTIONS CALLED:
%
%  ecef2ned
%
% MODIFICATIONS:    
% 
%       02-01-02  :  Lisa Reeh - Original
%       03-23-04  :  Stephen Russell - put position/velocity, visibility,
%                    and DOP calculations into their own separate functions
%       09-07-06:  Jan Weiss - Removed code for ASEN5090 Fall 2006 assignment.
% 
% 
% Colorado Center for Astrodynamics Research
% Copyright 2006 University of Colorado, Boulder
%*******************************************************

function azEl = getAzEl_5090(location, sat_pos)


% Add code to compute Az/El here







