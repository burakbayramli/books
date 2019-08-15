function a = bsp03a(A,B,S);
% Zermelo problem, cf. Bryson-Ho, p.66
% calculates angle of ship's axis for flow
% velocity v =(-S/2,0) and direct run from  A to B
C       =  B - A;
cos_psi = - C(1)/(C(1)^2 + C(2)^2);
psi     = acos(cos_psi);
d       = S*(cos_psi + sqrt(3 + cos_psi^2))/2;
cos_phi = (d^2 + 3*S^2/4)/(2*d*S);
phi     = acos(cos_phi);
a       = psi + phi;
