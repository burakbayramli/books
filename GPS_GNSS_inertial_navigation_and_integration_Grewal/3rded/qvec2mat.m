function quatmat = qvec2mat(quatvec)
%
% converts quaternion 4-vector to quaternion 4x4 matrix
%
quatmat = quatvec(1)*eye(4)+quatvec(2)*[0,-1,0,0;1,0,0,0;0,0,0,-1;0,0,1,0]+quatvec(3)*[0,0,-1,0;0,0,0,1;1,0,0,0;0,-1,0,0]+quatvec(4)*[0,0,0,-1;0,0,-1,0;0,1,0,0;1,0,0,0];
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
