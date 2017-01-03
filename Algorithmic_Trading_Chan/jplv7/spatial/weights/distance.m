function D = distance(xc,yc)
% PURPOSE: Computes the list of pairwise distances for a given set of locations (loc).
% ----------------------------------------------------------
% Usage: D = distance(xc,yc)
% where: xc,yc are vectors of latt-long coordinates for each location
% ----------------------------------------------------------
% Returns: D = (n x n)-matrix of pairwise distances

% Written by: TONY E. SMITH, 2/10/98

n = length(xc) ;  %number of locations
% Start procedure.
X = xc ; %column vector
Y = yc ; %column vector
U = ones(n,1) ; %column vector
XX = X*U' - U*X' ;
YY = Y*U' - U*Y' ;
D = (XX.^2 + YY.^2) ;

