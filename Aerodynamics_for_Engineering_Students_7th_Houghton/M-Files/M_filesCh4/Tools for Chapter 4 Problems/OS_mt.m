% OS_mt.m
% Usage: wave_angle = OS_mw(Mach number, turning angle<, gamma>) with all angles in degrees.
% Oblique shock equation.  No vector args because of solver.  
% Mach number and turning angle inputs, wave angle output
% S. Collicott
% (C) Copyright Purdue University, 2011
%
function [waveangle] = OS_mt(arg1, arg2, varargin)
gam = 1.4;  % default ratio of specific heats
waveangle = NaN;
Mach = arg1; % Mach number
turnangle = arg2;  % Wave angle in degrees
if isempty(find(Mach<1, 1)) && isempty(find(turnangle< 0, 1)) && length(varargin) < 2 && ...
  length(arg1)==1 && length(arg2)==1
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
  wavemaxrad = asin(sqrt((1/(gam*Mach^2))*...
    (0.25*(gam+1)*Mach^2+sqrt((gam+1)*(1+0.5*(gam-1)*Mach^2+0.0625*(gam+1)*Mach^4))-1)));
  turnmax = (180/pi)*atan( cot(wavemaxrad)*(Mach^2*sin(wavemaxrad)^2-1)/ ...
    (0.5*(gam+1)*Mach^2-(Mach^2*sin(wavemaxrad)^2-1)) );
  if turnangle <= turnmax
    %
    % create initial guess to make the function robust
    if turnmax - turnangle > 3
      waveinit = 0.8*wavemaxrad*180/pi;
    else
      waveinit = 0.9*wavemaxrad*180/pi;
    end
    %
    % Solve it
    waveangle = fzero(@(waveangle) ( cot(waveangle*pi/180)*(Mach^2*sin(waveangle*pi/180)^2-1)/ ...
    (0.5*(gam+1)*Mach^2-(Mach^2*sin(waveangle*pi/180)^2-1)) )-tan(turnangle*pi/180), waveinit);
  end
end
