function CNED2RPY = Euler2CTMat(roll,pitch,heading)
%
% CNED2RPY = Euler2CTMat(roll,pitch,heading)
%
% Converts from roll-pitch-yaw Euler angles to coordinate transformation
% matrix from North-East-Down coordinates to body-fixed Roll-Pitch-Yaw
% axis coordinates.
%
% INPUTS:
%    roll     = vehicle roll angle [rad]
%    pitch    = vehicle pitch angle [rad]
%    heading  = heading of roll axis measured clockwise from north [rad]
% OUTPUT:
%    CNED2RPY = 3-by-3 coordinate transformation from North-East-Down
%               coordinates to body-fixed roll-pitch-yaw coordinates
%
sr = sin(roll);
cr = cos(roll);
sp = sin(pitch);
cp = cos(pitch);
sy = sin(heading);
cy = cos(heading);
CNED2RPY = [cy*cp,sy*cp,-sp;cy*sp*sr-sy*cr,sy*sp*sr+cy*cr,cp*sr;cy*sp*cr+sy*sr,sy*sp*cr-cy*sr,cp*cr];
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  

