function y = LinearBarElementStiffness(E,A,L)
%LinearBarElementStiffness   This function returns the element 
%                            stiffness matrix for a linear bar with  
%                            modulus of elasticity E, cross-sectional 
%                            area A, and length L. The size of the  
%                            element stiffness matrix is 2 x 2.
y = [E*A/L -E*A/L ; -E*A/L E*A/L];


