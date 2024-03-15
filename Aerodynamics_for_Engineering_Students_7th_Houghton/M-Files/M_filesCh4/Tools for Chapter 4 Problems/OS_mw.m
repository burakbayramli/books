% OS_mw.m
% Usage: theta = OS_mw(Mach number, wave angle<, gamma>) with all angles in degrees.
% Oblique shock equation.
% Mach number and wave angle inputs,turning angle output
% S. Collicott
% (C) Copyright Purdue University, 2010
%
function [turnangle] = OS_mw(arg1, arg2, varargin)
gam = 1.4;  % default ratio of specific heats
turnangle = NaN;
Mach = arg1; % Mach number
waveangle = arg2;  % Wave angle in degrees
if isempty(find(Mach<1, 1)) && isempty(find(waveangle< (180/pi)*asin(1/Mach), 1)) && length(varargin) < 2
  if length(varargin)==1
    tmp = cell2mat(varargin);
    if tmp > 0
      gam = tmp;
    else
      gam = NaN;
    end
  end
  %
  turnangle = (180/pi)*atan((2./tan(waveangle*(pi/180)).*(Mach.^2.*sin(waveangle*(pi/180)).^2-1))...
    ./(Mach.^2*(gam+cos(2*waveangle*(pi/180)))+2));
end