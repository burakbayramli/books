function y = roundoff(number,decimal_places)
% PURPOSE: Rounds a number(vector) to a specified number of decimal places
% ----------------------------------------------------
% USAGE: y = roundoff(x,dplaces)
%        where: x       = a vector (or scalar)
%               dplaces = # of decimal places in y
% ----------------------------------------------------
% RETURNS: y = a vector (or scalar) with the
%          specified decimal places
% ----------------------------------------------------

% I don't know who wrote this one, but it comes
% in handy sometimes

 
 decimals = 10.^decimal_places;
 
 y = fix(decimals * number + 0.5)./decimals;
