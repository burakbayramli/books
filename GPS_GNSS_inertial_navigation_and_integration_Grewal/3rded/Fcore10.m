function Fcore = Fcore10(latitudeindegrees,vENU,tauAlt)
%
% function Fcore = Fcore10(latitudeindegrees,vENU)
%
% Dynamic coefficient matrix for 10-state INS error propagation,
% with state variables
%   1. Easting position error
%   2. Northing position error
%   3. Altitude error
%   4. East velocity error
%   5. North velocity error
%   6. Up velocity error
%   7. East-axis tilt error
%   8. North-axis tilt error
%   9. Heading error (counterclockwise)
%  10. Altimeter bias error [m]
%
%   INPUTS
%       latitudeindegrees INS latitude [deg]
%       vENU INS velocity relative to the earth, in ENU coordinates [m/s]
%       tauAlt altitude bias error correlation time-constant
%
%   OUTPUTS
%       Fcore   10x10 dynamic coefficient for inertial navigation errors in
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
Fcore9 = [0 0 0 1 0 0 0 0 0; 0 0 0 0 1 0 0 0 0; 0 0 0 0 0 1 0 0 0; 0 -2/REarth*OmegaEarth*s*vU-2/REarth*OmegaEarth*c*vN 0 0 -2*OmegaEarth*s 2*OmegaEarth*c -2*OmegaEarth*s*vU-2*OmegaEarth*c*vN -GM/REarth^2+OmegaEarth^2*c^2*REarth OmegaEarth^2*s*c*REarth; 0 2/REarth*OmegaEarth*c*vE+OmegaEarth^2*s^2-OmegaEarth^2*c^2 -OmegaEarth^2*s*c 2*OmegaEarth*s 0 0 GM/REarth^2+2*OmegaEarth*c*vE-OmegaEarth^2*c^2*REarth -2*OmegaEarth*s*vU 2*OmegaEarth*c*vU; 0 2/REarth*OmegaEarth*s*vE-2*OmegaEarth^2*s*c 2*GM/REarth^3+OmegaEarth^2*c^2 -2*OmegaEarth*c 0 0 2*OmegaEarth*s*vE-OmegaEarth^2*s*c*REarth 2*OmegaEarth*s*vN -2*OmegaEarth*c*vN; 0 0 1/REarth^2*vN 0 -1/REarth 0 -1/REarth*vU -OmegaEarth*s OmegaEarth*c; 0 1/REarth*OmegaEarth*s -1/REarth^2*vE 1/REarth 0 0 OmegaEarth*s -1/REarth*vU 0; 0 -1/REarth*OmegaEarth*c 0 0 0 0 -OmegaEarth*c+1/REarth*vE 1/REarth*vN 0];
Fcore  = [Fcore9,zeros(9,1);zeros(1,9),-1/tauAlt];
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  
