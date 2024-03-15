% OS_tmax.m
% Usage: turning_angle = OS_tmax(Mach number<, gamma>) with all angles in degrees.
% Oblique shock equation - max turning angle as functino of Mach number.  
% Mach number input, max turning angle output
% S. Collicott
% (C) Copyright Purdue University, 2011
%
function [turnmax] = OS_tmax(arg1, varargin)
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
  wavemaxrad = asin(sqrt((1./(gam*Mach.^2)).*...
    (0.25*(gam+1)*Mach.^2+sqrt((gam+1)*(1+0.5*(gam-1)*Mach.^2+0.0625*(gam+1)*Mach.^4))-1)));
  turnmax = (180/pi)*atan( cot(wavemaxrad).*(Mach.^2.*sin(wavemaxrad).^2-1) ./ ...
    (0.5*(gam+1)*Mach.^2-(Mach.^2.*sin(wavemaxrad).^2-1)) );
end
