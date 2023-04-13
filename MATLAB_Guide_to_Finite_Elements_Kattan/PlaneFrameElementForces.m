function y = PlaneFrameElementForces(E,A,I,L,theta,u)
%PlaneFrameElementForces   This function returns the element force
%                          vector given the modulus of elasticity E,
%                          the cross-sectional area A, the moment of 
%                          inertia I, the length L, the angle theta
%                          (in degrees), and the element nodal 
%                          displacement vector u.
x = theta * pi/180;
C = cos(x);
S = sin(x);
w1 = E*A/L;
w2 = 12*E*I/(L*L*L);
w3 = 6*E*I/(L*L);
w4 = 4*E*I/L;
w5 = 2*E*I/L;
kprime = [w1 0 0 -w1 0 0 ; 0 w2 w3 0 -w2 w3 ;
   0 w3 w4 0 -w3 w5 ; -w1 0 0 w1 0 0 ;
   0 -w2 -w3 0 w2 -w3 ; 0 w3 w5 0 -w3 w4];
T = [C S 0 0 0 0 ; -S C 0 0 0 0 ; 0 0 1 0 0 0 ;
   0 0 0 C S 0 ; 0 0 0 -S C 0 ; 0 0 0 0 0 1];
y = kprime*T* u;



