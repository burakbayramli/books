% NS_r2r1.m
% Usage: drat = NS_r2r1(Mach <, gamma>) where gamma=1.4 if omitted
% Function for normal shock static density ratio
% S. Collicott, Feb. 2009
% Copyright (C) 2010, Purdue University
%
function [drat] = NS_r2r1(arg1, varargin)
gam = 1.4;  % default ratio of specific heats
drat = NaN;
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
  drat = (gam+1)*Mach.^2 ./ (2+(gam-1)*Mach.^2);
end
