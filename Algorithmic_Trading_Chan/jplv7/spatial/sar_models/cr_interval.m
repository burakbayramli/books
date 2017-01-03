
function bounds = cr_interval(adraw,hperc)
% PURPOSE: Computes an hperc-percent credible interval for a vector of MCMC draws
% --------------------------------------------------------------------
% Usage: bounds = cr_interval(draws,hperc);
% where draws = an ndraw by nvar matrix
%       hperc = 0 to 1 value for hperc percentage point
% --------------------------------------------------------------------
% RETURNS:
%         bounds = a 1 x 2 vector with 
%         bounds(1,1) = 1-hperc percentage point
%         bounds(1,2) = hperc percentage point
%          e.g. if hperc = 0.95
%          bounds(1,1) = 0.05 point for 1st vector in the matrix
%          bounds(1,2) = 0.95 point  for 1st vector in the matrix
%          bounds(2,1) = 0.05 point for 2nd vector in the matrix
%          bounds(2,2) = 0.05 point for 2nd vector in the matrix
%          ...
% --------------------------------------------------------------------

% Written by J.P. LeSage

% This function takes a vector of MCMC draws and calculates
% an hperc-percent credible interval
[ndraw,ncols]=size(adraw);
botperc=round((0.50-hperc/2)*ndraw);
topperc=round((0.50+hperc/2)*ndraw);
bounds = zeros(ncols,2);
for i=1:ncols;
temp = sort(adraw(:,i),1);
bounds(i,:) =[temp(topperc,1) temp(botperc,1)];
end;

