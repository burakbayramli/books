function [roll,pitch,yaw] = CTMat2Euler(CTMat)
%
% [roll,pitch,yaw] = CTMat2RPY(CTMat)
%
% Converts from coordinate transformation matrix to roll-pitch-yaw
% Euler angles (yaw == heading) 
%
roll  = atan2(CTMat(2,3),CTMat(3,3));
pitch = asin(-CTMat(1,3));
yaw   = atan2(CTMat(1,2),CTMat(1,1));
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  
