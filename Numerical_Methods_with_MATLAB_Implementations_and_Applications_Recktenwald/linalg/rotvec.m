function  v = rotvec(u,alpha,beta,zeta)
% rotvec  Rotates a three dimensional vector
%
% Synopsis:  v = rotvec(u,alpha,beta,zeta)
%
% Input:     u     = initial vector
%            alpha = angle, in degrees, of rotation about the x-axis
%            beta  = angle, in degrees, of rotation about the y-axis
%            zeta  = angle, in degrees, of rotation about the z-axis
%
%  Output:  v = final vector, i.e. result of rotating u through
%               the angles alpha, beta and zeta

a = alpha*pi/180;  b = beta*pi/180;  z = zeta*pi/180;  % convert to radians

% --- set up rotation matrices
Rx = [ 1  0  0; 0  cos(a)  -sin(a); 0  sin(a)  cos(a)];
Ry = [ cos(b)  0  sin(b);  0  1  0; -sin(b)  0  cos(b) ];
Rz = [ cos(z) -sin(z)  0;  sin(z)  cos(z)  0;  0  0  1 ];

v = Rz*Ry*Rx*u;          %  apply the rotation
