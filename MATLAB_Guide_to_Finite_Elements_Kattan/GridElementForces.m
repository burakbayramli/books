function y = GridElementForces(E,G,I,J,L,theta,u)
%GridElementForces   This function returns the element force
%                    vector given the modulus of elasticity E,
%                    the shear modulus of elasticity G, the 
%                    moment of inertia I, the torsional constant J, 
%                    the length L, the angle theta (in degrees), 
%                    and the element nodal displacement vector u.
x = theta*pi/180;
C = cos(x);
S = sin(x);
w1 = 12*E*I/(L*L*L);
w2 = 6*E*I/(L*L);
w3 = G*J/L;
w4 = 4*E*I/L;
w5 = 2*E*I/L;
kprime = [w1 0 w2 -w1 0 w2 ; 0 w3 0 0 -w3 0 ;
   w2 0 w4 -w2 0 w5 ; -w1 0 -w2 w1 0 -w2 ;
   0 -w3 0 0 w3 0 ; w2 0 w5 -w2 0 w4];
R = [1 0 0 0 0 0 ; 0 C S 0 0 0 ; 0 -S C 0 0 0 ;
   0 0 0 1 0 0 ; 0 0 0 0 C S ; 0 0 0 0 -S C];
y = kprime*R* u;



