% NS_s2s1.m
% Usage: sdif = NS_s2s1(Mach <, gamma>) where gamma=1.4 if omitted.
% Function for normal shock static entropy change non-dimensionalized 
% by specific gas constant.
% S. Collicott, Sep. 2009
% Copyright (C) 2010, Purdue University
%
function [sdif] = NS_s2s1(arg1, varargin)
gam = 1.4;  % default ratio of specific heats
sdif = NaN;
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
  sdif = (gam-1)*log(1./NS_po2po1(Mach,gam));
end
