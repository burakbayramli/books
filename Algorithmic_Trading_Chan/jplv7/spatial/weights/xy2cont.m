function [wswdel,wwsdel,wmatdel]=xy2cont(xc,yc)
% PURPOSE: uses x,y coordinates to produce spatial contiguity weight matrices
%          with delaunay routine from MATLAB version 5.2
% ------------------------------------------------------
% USAGE: [w1 w2 w3] = xy2cont(xcoord,ycoord)
% where:     xcoord = x-direction coordinate
%            ycoord = y-direction coordinate
% ------------------------------------------------------
% RETURNS: 
%          w1 = W*S*W, a symmetric spatial weight matrix (max(eig)=1)
%          w2 = W*W*S, a row-stochastic spatial weight matrix, where S represents 
%               the adjacency matrix from Delaunay triangles (Voronoi tesselation).
%          w3 = diagonal matrix with i,i equal to 1/sqrt(sum of ith row)
% ------------------------------------------------------
% References: Kelley Pace, Spatial Statistics Toolbox 2.0
% ------------------------------------------------------

% Written by Kelley Pace, 6/23/97 
% revised on 12/25/02
% (named fdelw2 in his spatial statistics toolbox)
% Documentation modified by J. LeSage 11/2002

if nargin ~= 2
error('xy2cont: 2 input arguments required');
end;

% error checking on inputs
[n junk] = size(xc);
if junk ~= 1
xc = xc';
end;
[n2 junk] = size(yc);
if junk ~= 1
yc = yc';
end;
if n ~= n2
error('xy2cont: xc,yc inputs must be same size');
end;


[wswdel,wwsdel,wmatdel]=fdelw2(xc,yc);

