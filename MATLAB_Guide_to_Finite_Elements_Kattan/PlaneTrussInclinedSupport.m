function y = PlaneTrussInclinedSupport(T,i,alpha)
%PlaneTrussInclinedSupport   This function calculates the
%                            tranformation matrix T of the inclined
%                            support at node i with angle of
%                            inclination alpha (in degrees).
x = alpha*pi/180;
T(2*i-1,2*i-1) = cos(x);
T(2*i-1,2*i) = sin(x);
T(2*i,2*i-1) = -sin(x) ;
T(2*i,2*i) = cos(x);
y = T;



