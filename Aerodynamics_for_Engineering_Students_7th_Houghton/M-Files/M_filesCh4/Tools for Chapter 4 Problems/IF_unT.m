% IF_unT.m
% Usage: Mach = IF_unT(Tratio <, gamma>) where gamma=1.4 if omitted
% Function for finding Mach number from isentropic stagnation to static temperature ratio.
% S. Collicott, Oct. 2010
% Copyright (C) 2010, Purdue University
%
function [Mach] = IF_unT(arg1, varargin)
gam = 1.4;  % default ratio of specific heats
Mach = NaN;
Trat = arg1; % Mach number
if isempty(find(Trat < 1)) && length(varargin) < 2
  if length(varargin)==1
    tmp = cell2mat(varargin);
    if tmp > 0
      gam = tmp;
    else
      gam = NaN;
    end
  end
  %
  Mach = sqrt((Trat-1)*(2/(gam-1)));
end
