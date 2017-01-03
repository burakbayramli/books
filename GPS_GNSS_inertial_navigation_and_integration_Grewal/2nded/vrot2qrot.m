function quat = vrot2qrot(rotvec,quat0)
%function quat = vrot2qrot(rotvec,quat0)
%
% applies rotation vector to quaternion matrix
%
% INPUTS
%   rotvec  3x1 or 1x3 rotation vector
%   quat0   initial value 4x4 quaternion matrix
% OUTPUT
%   quat    rotated 4x4 quaternion matrix
%
quatvec = rotvec2qvec(rotvec)
quatmat = qvec2mat(quatvec);
quat    = quatmat*quat0/quatmat;