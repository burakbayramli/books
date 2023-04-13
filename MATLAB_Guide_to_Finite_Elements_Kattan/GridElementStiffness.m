function y = GridElementStiffness(E,G,I,J,L,theta)
%GridElementStiffness   This function returns the element 
%                       stiffness matrix for a grid   
%                       element with modulus of elasticity E,  
%                       shear modulus of elasticity G, moment of 
%                       inertia I, torsional constant J, length L,
%                       and angle theta (in degrees).
%                       The size of the element stiffness 
%                       matrix is 6 x 6.
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
y = R'*kprime*R;   





