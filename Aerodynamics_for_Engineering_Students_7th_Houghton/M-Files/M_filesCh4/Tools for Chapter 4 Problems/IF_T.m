% IF_T.m
% Usage: Trat = IF_T(Mach <, gamma>) where gamma=1.4 if omitted
% Function for isentropic flow stagnation to static temperature ratio.
% S. Collicott, Feb. 2009
% Copyright (C) 2010, Purdue University
%
function [Trat] = IF_T(arg1, varargin)
gam = 1.4;  % default ratio of specific heats
Trat = NaN;
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
  Trat = (1+0.5*(gam-1)*Mach.^2);
end
