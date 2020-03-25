function L=ellipsoid(v1,v2)
global a b c
A1=b*c*sin(v2).^2.*cos(v1);
B1=a*c*sin(v2).^2.*sin(v1);
C1=a*b*cos(v2).*sin(v2);
L=sqrt(A1.^2+B1.^2+C1.^2);

