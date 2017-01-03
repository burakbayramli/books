function Fcore = Fcore7(Lat,Vel)
%
% Grewal, Andrews, and Bartone,
% Global Navigation Satellite Systems, Inertial Navigation, and Integration
% 3rd Edition, Wiley, 2012
%
% function Fcore = Fcore7(Lat,Vel)
%
% Dynamic coefficient matrix for 7-state INS error propagation,
% with state variables
%   1. Easting position error
%   2. Northing position error
%   3. East velocity error
%   4. North velocity error
%   5. East-axis tilt rotation error
%   6. North-axis tilt rotation error
%   7. Heading error (counterclockwise)
%
%   INPUTS
%       Lat  Latitudeindegrees INS latitude [deg]
%       Vel  INS velocity reative to the earth, in ENU coordinates [m/s]
%
%   OUTPUTS
%       Fcore   7x7 dynamic coefficient for inertial navigation errors in
%               ENU coordinates and SI units
%
REarth = 0.6371009e7;  % [m]         mean Earth radius
OmegaEarth = 0.7292115e-4; % [rad/s]     Earth rotation rate
GM     = 0.3986004418e15;  % [m^3/s^2]   Earth gravitational constant
c      = cos(Lat*pi/180);
s      = sin(Lat*pi/180);
vE     = Vel(1);
vN     = Vel(2);
%
Fcore = [0 0 1 0 0 0 0; 0 0 0 1 0 0 0; 0 2*OmegaEarth/REarth*c*vN 0 2*OmegaEarth*s 2*OmegaEarth*c*vN -GM/REarth^2+OmegaEarth^2*c^2*REarth OmegaEarth^2*s*c*REarth; 0 -2*OmegaEarth/REarth*c*vE+OmegaEarth^2*s^2-OmegaEarth^2*c^2 -2*OmegaEarth*s 0 GM/REarth^2-2*OmegaEarth*c*vE-OmegaEarth^2*c^2*REarth 0 0; 0 0 0 -1/REarth 0 -OmegaEarth*s OmegaEarth*c; 0 OmegaEarth/REarth*s 1/REarth 0 OmegaEarth*s 0 0; 0 -OmegaEarth/REarth*c 0 0 -OmegaEarth*c+1/REarth*vE 1/REarth*vN 0];
