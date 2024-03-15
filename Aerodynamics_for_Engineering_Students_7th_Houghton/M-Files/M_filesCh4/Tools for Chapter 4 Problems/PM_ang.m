% PM_ang.m
% Usage: nu = PM_ang(Mach <, gamma>) where gamma=1.4 if omitted
% Function for computing Prandtl-Meyer angle from Mach number.f
% S. Collicott, Feb. 2009
% Copyright (C) 2010, Purdue University
%
function [nu] = PM_ang(arg1, varargin)
gam = 1.4;  % default ratio of specific heats
nu = NaN;
Mach = arg1; % Mach number
if isempty(find(Mach < 1)) && length(varargin) < 2
  if length(varargin)==1
    tmp = cell2mat(varargin);
    if tmp > 0
      gam = tmp;
    else
      gam = NaN;
    end
  end
  %
  nu = sqrt((gam+1)/(gam-1))*atan(sqrt((gam-1)*(Mach.^2-1)/(gam+1)))-atan(sqrt(Mach.^2-1));
  nu = nu * 180 / pi;
end
