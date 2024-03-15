% IF_unr.m
% Usage: Drat = IF_unr(Mach <, gamma>) where gamma=1.4 if omitted
% Function for Mach number from isentropic stagnation to static density ratio.
% S. Collicott, Oct. 2010
% Copyright (C) 2010, Purdue University
%
function [Mach] = IF_unr(arg1, varargin)
gam = 1.4;  % default ratio of specific heats
Mach = NaN;
Drat = arg1; % Mach number
if isempty(find(Drat < 0)) && length(varargin) < 2
  if length(varargin)==1
    tmp = cell2mat(varargin);
    if tmp > 0
      gam = tmp;
    else
      gam = NaN;
    end
  end
  %
  Mach = sqrt( (Drat.^(gam-1)-1)*(2/(gam-1)) );
end
