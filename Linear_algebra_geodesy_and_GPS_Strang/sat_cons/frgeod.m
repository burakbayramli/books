function [X,Y,Z] = frgeod(a,finv,dphi,dlambda,h)
%FRGEOD  Subroutine to calculate Cartesian coordinates X,Y,Z
%	      given geodetic coordinates latitude, longitude (east),
%	      and height above reference ellipsoid along with
%	      reference ellipsoid values semi-major axis (a) and
%	      the inverse of flattening (finv)

% The units of linear parameters h,a must agree (m,km,mi,..etc).
% The input units of angular quantities must be in decimal degrees.
% The output units of X,Y,Z will be the same as the units of h and a.

% Copyright (C) 1987 C. Goad, Columbus, Ohio
% Reprinted with permission of author, 1996
% Original Fortran code rewritten into MATLAB
% Kai Borre 03-03-96

% compute degree-to-radian factor
dtr = pi/180;
% compute square of eccentricity
esq = (2-1/finv)/finv;
sinphi = sin(dphi*dtr);
% compute radius of curvature in prime vertical
N_phi = a/sqrt(1-esq*sinphi*sinphi);
% compute P and Z
% P is distance from Z axis
P = (N_phi + h)*cos(dphi*dtr);
Z = (N_phi*(1-esq) + h) * sinphi;
X = P*cos(dlambda*dtr);
Y = P*sin(dlambda*dtr);
%%%%%%% end frgeod.m %%%%%%%%%%%%%%
