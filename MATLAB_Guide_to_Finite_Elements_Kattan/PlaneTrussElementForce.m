function y = PlaneTrussElementForce(E,A,L,theta,u)
%PlaneTrussElementForce   This function returns the element force
%                         given the modulus of elasticity E, the 
%                         cross-sectional area A, the length L, 
%                         the angle theta (in degrees), and the 
%                         element nodal displacement vector u.
x = theta * pi/180;
C = cos(x);
S = sin(x);
y = E*A/L*[-C -S C S]* u;


