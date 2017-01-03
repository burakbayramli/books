function CNED2RPY = NED2RPY(Roll,Pitch,Heading)
%
% Computes orthogonal transformation matrix from locally-level
% North-East-Down coordinates to vehicle-fixed Roll-Pitch-Yaw coordinates
%
% INPUTS are Euler angles in radians
%   Roll    vehicle roll angle (right wing down)
%   Pitch   vehicle pitch angle (nose up)
%   Heading vehicle heading (clockwise from north)
% OUTPUT
%   CNED2RPY 3x3 coordinate transformation matrix from locally-level
%   North-East-Down coordinates to vehicle-fixed Roll-Pitch-Yaw coordinates
%
sR = sin(Roll);
cR = cos(Roll);
sP = sin(Pitch);
cP = cos(Pitch);
sH = sin(Heading);
cH = cos(Heading);
CR = [ 1 , 0 , 0 ; 0 , cR , sR ; 0 , -sR , cR ] ;
CP = [ cP , 0 , -sP ; 0 , 1 , 0 ; sP , 0 , cP ] ;
CH = [ cH , sH , 0 ; -sH , cH , 0 ; 0 , 0 , 1 ] ;
CNED2RPY = CR * CP * CH;
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
