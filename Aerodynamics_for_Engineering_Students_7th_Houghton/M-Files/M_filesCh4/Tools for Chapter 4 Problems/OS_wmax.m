% OS_wmax.m
% Usage: wave_angle = OS_wmax(Mach number<, gamma>) with all angles in degrees.
% Oblique shock equation - wave angle at max turning angle.  
% Mach number input, wave angle at max turnign angle output
% S. Collicott
% (C) Copyright Purdue University, 2011
%
function [waveangle] = OS_wmax(arg1, varargin)
gam = 1.4;  % default ratio of specific heats
waveangle = NaN;
Mach = arg1; % Mach number
if isempty(find(Mach<1, 1)) && length(varargin) < 2 
  if length(varargin)==1
    tmp = cell2mat(varargin);
    if tmp > 0
      gam = tmp;
    else
      gam = NaN;
    end
  end
  %
  % Check max turning angle
  waveangle = (180/pi)*asin(sqrt((1./(gam*Mach.^2)).*...
    (0.25*(gam+1)*Mach.^2+sqrt((gam+1)*(1+0.5*(gam-1)*Mach.^2+0.0625*(gam+1)*Mach.^4))-1)));
end
