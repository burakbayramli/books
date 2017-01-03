function Crpy2ned = RPY2NED(roll,pitch,yaw)
%
% Computes transformation matrix from Roll-Pitch-Yaw coordinates to
% North-East-Down coordinates
%
% INPUTS:
%    roll   rollangle in radians
%    pitch  pitch angle in radians
%    yaw    yaw (heading clockwise from north) in radians
% OUTPUT:
%    Crpy2ned 3-by-3 coordinate transformation matrix
%
sr = sin(roll);
cr = cos(roll);
sp = sin(pitch);
cp = cos(pitch);
sy = sin(yaw);
cy = cos(yaw);
Crpy2ned = [cy*cp,sy*cp,-sp;-sy*cr+cy*sp*sr,cy*cr+sy*sp*sr,cp*sr;sy*sr+cy*sp*cr,-cy*sr+sy*sp*cr,cp*cr]';
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
