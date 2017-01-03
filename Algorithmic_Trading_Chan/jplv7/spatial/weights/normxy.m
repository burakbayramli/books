function [teast,tnorth] = normxy(east,north)
% PURPOSE: Perform isotropic normalization of x-y coordinates
%---------------------------------------------------
% USAGE: [easto northo] = normxy(east,north)
% where:   east  = longitude (x-direction) coordinates
%          north = lattitude (y-direction) coordinates
%---------------------------------------------------
% RETURNS: easto  = normalized east coordinate
%          northo = normalized north coordinate
%---------------------------------------------------
% coded from FORTRAN code supplied
% by Brunsdon, Fotheringham, Charlton 

[nobs junk] = size(east);

% compute means
xbar = mean(east);
ybar = mean(north);

% compute variance
xvar = std(east)*std(east);
yvar = std(north)*std(north);

% do normalization
sdist = sqrt(xvar+yvar);

teast = east - xbar*ones(nobs,1);
tnorth = north - ybar*ones(nobs,1);
teast = teast*(1/sdist);
tnorth = tnorth*(1/sdist);


