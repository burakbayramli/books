function y = TetrahedronElementStiffness(E,NU,x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4)
%TetrahedronElementStiffness   This function returns the element 
%                              stiffness matrix for a linear   
%                              tetrahedral (solid) element with
%                              modulus of elasticity E, 
%                              Poisson's ratio NU, coordinates 
%                              of the first node (x1,y1,z1), 
%                              coordinates of the second node 
%                              (x2,y2,z2), coordinates of the 
%                              third node (x3,y3,z3), and 
%                              coordinates of the fourth node
%                              (x4,y4,z4).
%                              The size of the element stiffness 
%                              matrix is 12 x 12.
xyz = [1 x1 y1 z1 ; 1 x2 y2 z2 ; 1 x3 y3 z3 ; 1 x4 y4 z4];
V = det(xyz)/6;
mbeta1 = [1 y2 z2 ; 1 y3 z3 ; 1 y4 z4];
mbeta2 = [1 y1 z1 ; 1 y3 z3 ; 1 y4 z4];
mbeta3 = [1 y1 z1 ; 1 y2 z2 ; 1 y4 z4];
mbeta4 = [1 y1 z1 ; 1 y2 z2 ; 1 y3 z3];
mgamma1 = [1 x2 z2 ; 1 x3 z3 ; 1 x4 z4];
mgamma2 = [1 x1 z1 ; 1 x3 z3 ; 1 x4 z4];
mgamma3 = [1 x1 z1 ; 1 x2 z2 ; 1 x4 z4];
mgamma4 = [1 x1 z1 ; 1 x2 z2 ; 1 x3 z3];
mdelta1 = [1 x2 y2 ; 1 x3 y3 ; 1 x4 y4];
mdelta2 = [1 x1 y1 ; 1 x3 y3 ; 1 x4 y4];
mdelta3 = [1 x1 y1 ; 1 x2 y2 ; 1 x4 y4];
mdelta4 = [1 x1 y1 ; 1 x2 y2 ; 1 x3 y3];
beta1 = -1*det(mbeta1);
beta2 = det(mbeta2);
beta3 = -1*det(mbeta3);
beta4 = det(mbeta4);
gamma1 = det(mgamma1);
gamma2 = -1*det(mgamma2);
gamma3 = det(mgamma3);
gamma4 = -1*det(mgamma4);
delta1 = -1*det(mdelta1);
delta2 = det(mdelta2);
delta3 = -1*det(mdelta3);
delta4 = det(mdelta4);
B1 = [beta1 0 0 ; 0 gamma1 0 ; 0 0 delta1 ; 
   gamma1 beta1 0 ; 0 delta1 gamma1 ; delta1 0 beta1];
B2 = [beta2 0 0 ; 0 gamma2 0 ; 0 0 delta2 ; 
   gamma2 beta2 0 ; 0 delta2 gamma2 ; delta2 0 beta2];
B3 = [beta3 0 0 ; 0 gamma3 0 ; 0 0 delta3 ; 
   gamma3 beta3 0 ; 0 delta3 gamma3 ; delta3 0 beta3];
B4 = [beta4 0 0 ; 0 gamma4 0 ; 0 0 delta4 ; 
   gamma4 beta4 0 ; 0 delta4 gamma4 ; delta4 0 beta4];
B = [B1 B2 B3 B4]/(6*V);
D = (E/((1+NU)*(1-2*NU)))*[1-NU NU NU 0 0 0 ; NU 1-NU NU 0 0 0 ; NU NU 1-NU 0 0 0 ;
   0 0 0 (1-2*NU)/2 0 0 ; 0 0 0 0 (1-2*NU)/2 0 ; 0 0 0 0 0 (1-2*NU)/2];
y = V*B'*D*B;

