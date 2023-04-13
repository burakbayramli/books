function y = PlaneFrameInclinedSupport(T,i,alpha)
%PlaneFrameInclinedSupport   This function calculates the
%                            tranformation matrix T of the inclined
%                            support at node i with angle of
%                            inclination alpha (in degrees).
x = alpha*pi/180;
T(3*i-2,3*i-2) = cos(x);
T(3*i-2,3*i-1) = sin(x);
T(3*i-2,3*i) = 0;
T(3*i-1,3*i-2) = -sin(x);
T(3*i-1,3*i-1) = cos(x);
T(3*i-1,3*i) = 0;
T(3*i,3*i-2) = 0;
T(3*i,3*i-1) = 0;
T(3*i,3*i) = 1;
y = T;



