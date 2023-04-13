function y = SpaceTrussElementStiffness(E,A,L,thetax,thetay,thetaz)
%SpaceTrussElementStiffness   This function returns the element 
%                             stiffness matrix for a space truss   
%                             element with modulus of elasticity E,  
%                             cross-sectional area A, length L, and
%                             angles thetax, thetay, thetaz 
%                             (in degrees). The size of the element 
%                             stiffness matrix is 6 x 6.
x = thetax*pi/180;
u = thetay*pi/180;
v = thetaz*pi/180;
Cx = cos(x);
Cy = cos(u);
Cz = cos(v);
w = [Cx*Cx Cx*Cy Cx*Cz ; Cy*Cx Cy*Cy Cy*Cz ; Cz*Cx Cz*Cy Cz*Cz];
y = E*A/L*[w -w ; -w w];




