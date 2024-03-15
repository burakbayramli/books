% NS_T2T1.m
% Usage: Trat = NS_T2T1(Mach <, gamma>) where gamma=1.4 if omitted
% Function for normal shock static temerature ratio
% Copyright (C) 2010, Purdue University
% S. Collicott, Feb. 2009
%
function [Trat] = NS_T2T1(arg1, varargin)
gam = 1.4;  % default ratio of specific heats
Trat = NaN;
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
  Trat = NS_p2p1(Mach,gam) ./ NS_r2r1(Mach,gam);
end
