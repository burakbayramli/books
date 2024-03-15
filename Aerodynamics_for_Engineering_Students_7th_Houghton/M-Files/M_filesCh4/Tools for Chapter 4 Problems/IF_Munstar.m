% IF_Munstar.m
% Usage: drat = IF_Munstar(Characteristic Mach <, gamma>) where gamma=1.4 if omitted
% Function for Mach number from characteristic Mach number.
% S. Collicott, Sep. 2009
% Copyright (C) 2010, Purdue University
%
function [M] = IF_Munstar(arg1, varargin)
gam = 1.4;  % default ratio of specific heats
M = NaN;
Mchar = arg1; % Mach number
if isempty(find(Mchar < 0)) && length(varargin) < 2
  if length(varargin)==1
    tmp = cell2mat(varargin);
    if tmp > 0
      gam = tmp;
    else
      gam = NaN;
    end
  end
  %
  if isempty( find(Mchar > sqrt((gam+1)/(gam-1)) ) ) 
    M = sqrt(2*Mchar.^2./( (gam+1)-(gam-1)*Mchar.^2));
  end
end
