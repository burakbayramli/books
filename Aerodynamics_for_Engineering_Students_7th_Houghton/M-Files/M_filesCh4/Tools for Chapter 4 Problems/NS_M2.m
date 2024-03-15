% NS_M2.m
% Usage: sdif = NS_M2(Mach <, gamma>) where gamma=1.4 if omitted
% Function for normal shock downstream Mach number.
% S. Collicott, Feb. 2009
% Copyright (C) 2010, Purdue University
%
function [M2] = NS_M2(arg1, varargin)
gam = 1.4;  % default ratio of specific heats
M2 = NaN;
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
  M2 = sqrt( (1+0.5*(gam-1)*Mach.^2) ./ (gam*Mach.^2 - 0.5*(gam-1)) );
end
