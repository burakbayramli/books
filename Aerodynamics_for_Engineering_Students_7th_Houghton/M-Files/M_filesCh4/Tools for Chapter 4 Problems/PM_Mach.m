% PM_Mach.m
% Usage: Mach = PM_Mach(angle <, gamma>) where gamma=1.4 if omitted, angle
% in degrees
% Function for computing Mach number from Prandtl-Meyer angle.
% S. Collicott, Feb. 2009
% Copyright (C) 2010, Purdue University
%
function [Mach] = PM_Mach(arg1, varargin)
gam = 1.4;  % default ratio of specific heats
Mach = NaN;
nu = arg1*pi/180; % Mach number
if isempty(find(nu<0)) && length(varargin) < 2
  if length(varargin)==1
    tmp = cell2mat(varargin);
    if tmp > 0
      gam = tmp;
    else
      gam = NaN;
    end
  end
  %
  if nu < pi/18
    Mguess = 1.5;
  elseif nu < 70*pi/180
    Mguess = 3.5;
  elseif nu < pi/1.8
    Mguess = 10;
  else
    Mguess = 35;
  end
  Mach = fzero( inline(sprintf(...
    '%g-(sqrt((%g+1)/(%g-1))*atan(sqrt((%g-1)*(Mach^2-1)/(%g+1)))-atan(sqrt(Mach^2-1)))',...
    nu, gam, gam, gam, gam),'Mach'), Mguess );
end
