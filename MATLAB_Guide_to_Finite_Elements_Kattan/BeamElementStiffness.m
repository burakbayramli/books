function y = BeamElementStiffness(E,I,L)
%BeamElementStiffness   This function returns the element 
%                       stiffness matrix for a beam   
%                       element with modulus of elasticity E,  
%                       moment of inertia I, and length L.
%                       The size of the element stiffness 
%                       matrix is 4 x 4.
y = E*I/(L*L*L)*[12 6*L -12 6*L ; 6*L 4*L*L -6*L 2*L*L ;
   -12 -6*L 12 -6*L ; 6*L 2*L*L -6*L 4*L*L];





