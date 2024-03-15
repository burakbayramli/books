% AreaMach.m
% Usage: Arat = AreaMach(Mach <, gamma>) where gamma=1.4 if omitted
% Function for Area-Mach number relation for nozzles
% S. Collicott, Feb. 2009
% Copyright (C) 2010, Purdue University
%
function [Arat] = AreaMach(arg1, varargin)
gam = 1.4;  % default ratio of specific heats
Aprat = NaN;
Mach = arg1; % Mach number
if isempty(find(Mach < 0)) && length(varargin) < 2
  if length(varargin)==1
    tmp = cell2mat(varargin);
    if tmp > 0
      gam = tmp;
    else
      gam = NaN;
    end
  end
  %
Arat = sqrt( ((2/(gam+1))*(1+0.5*(gam-1)*Mach.^2)).^((gam+1)/(gam-1)) )./Mach;
end
