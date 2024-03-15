% NS_p2p1.m
% Usage: prat = NS_p2p1(Mach <, gamma>) where gamma=1.4 if omitted
% Function for normal shock static pressure ratio
% S. Collicott, Feb. 2009
% Copyright (C) 2010, Purdue University
%
function [prat] = NS_p2p1(arg1, varargin)
gam = 1.4;  % default ratio of specific heats
prat = NaN;
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
  prat = 1+(Mach.^2-1)*2*gam/(gam+1);
end
