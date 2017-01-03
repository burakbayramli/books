function Fcore = Fcore9(latitudeindegrees,vENU,aENU)
%
% function Fcore = Fcore9(latitudeindegrees,vENU,aENU)
%   INPUTS
%       latitudeindegrees INS latitude [deg]
%       vENU INS velocity reative to the earth, in ENU coordinates [m/s]
%
%   OUTPUTS
%       Fcore   9x9 dynamic coefficient for inertial navigation errors in
%               ENU coordinates and SI units
%
REarth = 0.6371009e7;  % [m]         mean Earth radius
OmegaEarth = 0.7292115e-4; % [rad/s]     Earth rotation rate
GM     = 0.3986004e15; % [m^3/s^2]   Earth gravitational constant
c      = cos(latitudeindegrees*pi/180);
s      = sin(latitudeindegrees*pi/180);
vE     = vENU(1);
vN     = vENU(2);
vU     = vENU(3);
Fcore = [0 0 0 1 0 0 0 0 0; 0 0 0 0 1 0 0 0 0; 0 0 0 0 0 1 0 0 0; 0 2*OmegaEarth/REarth*s*vU+2*OmegaEarth/REarth*c*vN 0 0 2*OmegaEarth*s -2*OmegaEarth*c 2*OmegaEarth*s*vU+2*OmegaEarth*c*vN -GM/REarth^2+OmegaEarth^2*c^2*REarth OmegaEarth^2*s*c*REarth; 0 -2*OmegaEarth/REarth*c*vE+OmegaEarth^2*s^2-OmegaEarth^2*c^2 -OmegaEarth^2*s*c -2*OmegaEarth*s 0 0 GM/REarth^2-2*OmegaEarth*c*vE-OmegaEarth^2*c^2*REarth 2*OmegaEarth*s*vU -2*OmegaEarth*c*vU; 0 -2*OmegaEarth/REarth*s*vE-2*OmegaEarth^2*s*c 2*GM/REarth^3+OmegaEarth^2*c^2 2*OmegaEarth*c 0 0 -2*OmegaEarth*s*vE-OmegaEarth^2*s*c*REarth -2*OmegaEarth*s*vN 2*OmegaEarth*c*vN; 0 0 1/REarth^2*vN 0 -1/REarth 0 -1/REarth*vU -OmegaEarth*s OmegaEarth*c; 0 OmegaEarth/REarth*s -1/REarth^2*vE 1/REarth 0 0 OmegaEarth*s -1/REarth*vU 0; 0 -OmegaEarth/REarth*c 0 0 0 0 -OmegaEarth*c+1/REarth*vE 1/REarth*vN 0];
Fcore(4,8) = Fcore(4,8) - aENU(3);
Fcore(4,9) = Fcore(4,9) + aENU(2);
Fcore(5,7) = Fcore(5,7) + aENU(3);
Fcore(5,9) = Fcore(5,9) - aENU(1);
Fcore(6,7) = Fcore(6,7) - aENU(2);
Fcore(6,8) = Fcore(6,8) + aENU(1);
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  
