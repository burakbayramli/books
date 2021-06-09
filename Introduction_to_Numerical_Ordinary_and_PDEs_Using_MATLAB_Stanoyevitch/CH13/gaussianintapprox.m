function int = gaussianintapprox(f,V1,V2,V3)
% M-file for numerically approximating integral of a function f(x,y)
% over a triangle in the plane with vertices V1, V2, V3
% Approximation is done using the Gaussian quadrature formula (24)
% of Chapter 13.
% Input Varialbes:  f = an inline function or an M-file of the integrand
% specified as a function of two variables: x and y
% V1, V2, V3 length 2 row vectors containing coordinates of the vertices
% of the triangle.  Output variable: int = approximation
A=feval(f,(V1(1)+V2(1))/2,(V1(2)+V2(2))/2);
B=feval(f,(V1(1)+V3(1))/2,(V1(2)+V3(2))/2);
C=feval(f,(V2(1)+V3(1))/2,(V2(2)+V3(2))/2);
M=[V1 1;,V2 1; V3 1];
area=abs(det(M))/2;  %See formula (5) of Chapter 13
int=area*(A+B+C)/3;