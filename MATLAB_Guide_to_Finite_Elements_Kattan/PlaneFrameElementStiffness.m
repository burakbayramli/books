function y = PlaneFrameElementStiffness(E,A,I,L,theta)
%PlaneFrameElementStiffness   This function returns the element 
%                             stiffness matrix for a plane frame   
%                             element with modulus of elasticity E,  
%                             cross-sectional area A, moment of 
%                             inertia I, length L, and angle 
%                             theta (in degrees).
%                             The size of the element stiffness 
%                             matrix is 6 x 6.
x = theta*pi/180;
C = cos(x);
S = sin(x);
w1 = A*C*C + 12*I*S*S/(L*L);
w2 = A*S*S + 12*I*C*C/(L*L);
w3 = (A-12*I/(L*L))*C*S;
w4 = 6*I*S/L;
w5 = 6*I*C/L;
y = E/L*[w1 w3 -w4 -w1 -w3 -w4 ; w3 w2 w5 -w3 -w2 w5 ;
   -w4 w5 4*I w4 -w5 2*I ; -w1 -w3 w4 w1 w3 w4 ;
   -w3 -w2 -w5 w3 w2 -w5 ; -w4 w5 2*I w4 -w5 4*I];





