function y = SpaceTrussElementStress(E,L,thetax,thetay,thetaz,u)
%SpaceTrussElementStress   This function returns the element stress
%                          given the modulus of elasticity E, the 
%                          length L, the angles thetax, thetay, 
%                          thetaz (in degrees), and the element 
%                          nodal displacement vector u.
x = thetax * pi/180;
w = thetay * pi/180;
v = thetaz * pi/180;
Cx = cos(x);
Cy = cos(w);
Cz = cos(v);
y = E/L*[-Cx -Cy -Cz Cx Cy Cz]*u;







