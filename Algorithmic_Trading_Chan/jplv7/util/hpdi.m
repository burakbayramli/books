function bounds = hpdi(adraw,hperc)
% PURPOSE: Computes an hperc-percent HPDI for a vector of MCMC draws
% --------------------------------------------------------------------
% Usage: bounds = hpdi(draws,hperc);
% where draws = an ndraw by 1 vector
%       hperc = 0 to 1 value for hperc percentage point
% --------------------------------------------------------------------
% RETURNS:
%         bounds = a 1 x 2 vector with 
%         bounds(1,1) = 1-hperc percentage point
%         bounds(1,2) = hperc percentage point
%          e.g. if hperc = 0.95
%          bounds(1,1) = 0.05 point
%          bounds(1,2) = 0.95 point
% --------------------------------------------------------------------

% Written by Gary Koop
% documented by J.P. LeSage

% This function takes a vector of MCMC draws and calculates
%a hperc-percent HPDI
ndraw=size(adraw,1);
botperc=round((0.50-hperc/2)*ndraw);
topperc=round((0.50+hperc/2)*ndraw);
temp = sort(adraw,1);
bounds=[temp(topperc,1) temp(botperc,1)];

