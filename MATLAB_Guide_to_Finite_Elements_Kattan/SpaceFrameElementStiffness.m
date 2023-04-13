function y = SpaceFrameElementStiffness(E,G,A,Iy,Iz,J,x1,y1,z1,x2,y2,z2)
%SpaceFrameElementStiffness   This function returns the element 
%                             stiffness matrix for a space frame   
%                             element with modulus of elasticity E,  
%                             shear modulus of elasticity G, cross-
%                             sectional area A, moments of inertia 
%                             Iy and Iz, torsional constant J, 
%                             coordinates (x1,y1,z1) for the first 
%                             node and coordinates (x2,y2,z2) for the
%                             second node.
%                             The size of the element stiffness 
%                             matrix is 12 x 12.
L = sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) + (z2-z1)*(z2-z1));
w1 = E*A/L;
w2 = 12*E*Iz/(L*L*L);
w3 = 6*E*Iz/(L*L);
w4 = 4*E*Iz/L;
w5 = 2*E*Iz/L;
w6 = 12*E*Iy/(L*L*L);
w7 = 6*E*Iy/(L*L);
w8 = 4*E*Iy/L;
w9 = 2*E*Iy/L;
w10 = G*J/L;
kprime = [w1 0 0 0 0 0 -w1 0 0 0 0 0 ;
   0 w2 0 0 0 w3 0 -w2 0 0 0 w3 ;
   0 0 w6 0 -w7 0 0 0 -w6 0 -w7 0 ;
   0 0 0 w10 0 0 0 0 0 -w10 0 0 ;
   0 0 -w7 0 w8 0 0 0 w7 0 w9 0 ;
   0 w3 0 0 0 w4 0 -w3 0 0 0 w5 ;
   -w1 0 0 0 0 0 w1 0 0 0 0 0 ;
   0 -w2 0 0 0 -w3 0 w2 0 0 0 -w3 ;
   0 0 -w6 0 w7 0 0 0 w6 0 w7 0 ;
   0 0 0 -w10 0 0 0 0 0 w10 0 0 ;
   0 0 -w7 0 w9 0 0 0 w7 0 w8 0 ;
   0 w3 0 0 0 w5 0 -w3 0 0 0 w4];
if x1 == x2 & y1 == y2
   if z2 > z1
      Lambda = [0 0 1 ; 0 1 0 ; -1 0 0];
   else
      Lambda = [0 0 -1 ; 0 1 0 ; 1 0 0];
   end
else
   CXx = (x2-x1)/L;
	CYx = (y2-y1)/L;
	CZx = (z2-z1)/L;
	D = sqrt(CXx*CXx + CYx*CYx);
	CXy = -CYx/D;
	CYy = CXx/D;
	CZy = 0;
	CXz = -CXx*CZx/D;
	CYz = -CYx*CZx/D;
	CZz = D;
	Lambda = [CXx CYx CZx ; CXy CYy CZy ; CXz CYz CZz];
end
R = [Lambda zeros(3) zeros(3) zeros(3) ; 
   zeros(3) Lambda zeros(3) zeros(3) ;
   zeros(3) zeros(3) Lambda zeros(3) ;
   zeros(3) zeros(3) zeros(3) Lambda];
y = R'*kprime*R;   





