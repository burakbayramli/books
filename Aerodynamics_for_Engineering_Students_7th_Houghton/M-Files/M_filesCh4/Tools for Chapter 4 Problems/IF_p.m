% IF_p.m
% Usage: Mach = IF_p(Pratio <, gamma>) where gamma=1.4 if omitted
% Function for Mach number from isentropic stagnation to static pressure ratio.
% S. Collicott, Oct. 2010
% Copyright (C) 2010, Purdue University
%
function [prat] = IF_p(arg1, varargin)
gam = 1.4;  % default ratio of specific heats
prat = NaN;
Mach = arg1; % Mach number
if isempty(find(Mach < 0, 1)) && length(varargin) < 2
  if length(varargin)==1
    tmp = cell2mat(varargin);
    if tmp > 0
      gam = tmp;
    else
      gam = NaN;
    end
  end
  %
  prat = (1+(gam-1)*0.5*Mach.^2).^(gam/(gam-1));
end
