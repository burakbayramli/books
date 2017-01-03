function quatvec = rotvec2qvec(rotvec)
%
% converts rotation vector to quaternion vector
%
rvec        = rotvec;
[rows,cols] = size(rvec);
if (cols == 3)&(rows == 1)
   rvec = rvec';
elseif (rows ~= 3)|(cols ~=1)
   error('Error in rotvec2qvec: rotation vector not 3x1 or 1x3');
end;
angle       = norm(rvec);
uvec        = rvec/angle;
quatvec     = [cos(angle/2);sin(angle/2)*uvec];
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
