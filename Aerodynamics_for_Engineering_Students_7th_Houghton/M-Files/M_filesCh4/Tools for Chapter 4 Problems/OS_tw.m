% OS_tw.m
% Usage: Mach = OS_tw(turning_angle, wave_angle <, gamma>) with all angles in degrees.
% Oblique shock equation.
% Turning angle and wave angle inputs, upstream Mach number output
% S. Collicott
% (C) Copyright Purdue University, 2010
%
function [Mach] = OS_tw(arg1, arg2, varargin)
gam = 1.4;  % default ratio of specific heats
Mach = NaN;
turnangle = arg1; % Mach number
waveangle = arg2;  % Wave angle in degrees
if isempty(find(turnangle<0, 1)) && isempty(find(turnangle>46, 1)) && isempty(find(waveangle< (180/pi)*asin(1/Mach), 1)) && length(varargin) < 2
  if length(varargin)==1
    tmp = cell2mat(varargin);
    if tmp > 0
      gam = tmp;
    else
      gam = NaN;
    end
  end
  %
 cotcot = cot(waveangle*(pi/180)).*cot(turnangle*(pi/180));
 Mach = sqrt(-2*(cotcot+1)./(gam + cos(2*waveangle*(pi./180))-2*cotcot.*sin(waveangle*(pi/180)).^2));
end