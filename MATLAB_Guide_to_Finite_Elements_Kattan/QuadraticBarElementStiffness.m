function y = QuadraticBarElementStiffness(E,A,L)
%QuadraticBarElementStiffness   This function returns the element 
%                               stiffness matrix for a quadratic bar  
%                               with modulus of elasticity E,  
%                               cross-sectional area A, and length L.   
%                               The size of the element stiffness 
%                               matrix is 3 x 3.
y = E*A/(3*L)*[7 1 -8 ; 1 7 -8 ; -8 -8 16];



