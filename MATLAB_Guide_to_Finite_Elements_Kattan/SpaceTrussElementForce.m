function y = SpaceTrussElementForce(E,A,L,thetax,thetay,thetaz,u)
%SpaceTrussElementForce   This function returns the element force
%                         given the modulus of elasticity E, the 
%                         cross-sectional area A, the length L, 
%                         the angles thetax, thetay, thetaz
%                         (in degrees), and the element nodal 
%                         displacement vector u.
x = thetax * pi/180;
w = thetay * pi/180;
v = thetaz * pi/180;
Cx = cos(x);
Cy = cos(w);
Cz = cos(v);
y = E*A/L*[-Cx -Cy -Cz Cx Cy Cz]*u;




