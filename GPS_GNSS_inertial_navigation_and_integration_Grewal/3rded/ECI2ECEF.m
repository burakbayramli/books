function CECI2ECEF = ECI2ECEF(t),
%
% Computes the coordinate transformation matrix from 
% Earth-Centered_Inertial (ECI) coordinates to
% Earth-Centered_Earth_Fixed (ECEF) coordinates.
%
% INPUTS
%
%   t  = time since the Vernal Equinox was at the Prime Meridian [s]
%
% OUTPUT
%
%   CECI2ECEF, 3x3 coordinate transformation matrix from ECI coordinates
%   to ECEF coordinates.
%
OmegaEarth = 0.7292115e-4; % [rad/s]     Earth rotation rate
RotAngle = t*OmegaEarth;   % Earth rotation since t=0
sA =sin(RotAngle);
cA =cos(RotAngle);
CECI2ECEF = [cA,sA,0;-sA,cA,0;0,0,1];
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  
