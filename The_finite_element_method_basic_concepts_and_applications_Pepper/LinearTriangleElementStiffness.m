function y = LinearTriangleElementStiffness(E,NU,t,xi,yi,xj,yj,xm,ym,p)
%LinearTriangleElementStiffness   This function returns the element 
%                                 stiffness matrix for a linear   
%                                 triangular element with modulus of 
%                                 elasticity E, Poisson's ratio NU,  
%                                 thickness t, coordinates of the 
%                                 first node (xi,yi), coordinates of 
%                                 the second node (xj,yj), and 
%                                 coordinates of the third node 
%                                 (xm,ym). Use p = 1 for cases of 
%                                 plane stress, and p = 2 for cases 
%                                 of plane strain.
%                                 The size of the element stiffness 
%                                 matrix is 6 x 6.
A = (xi*(yj-ym) + xj*(ym-yi) + xm*(yi-yj))/2;
betai = yj-ym;
betaj = ym-yi;
betam = yi-yj;
gammai = xm-xj;
gammaj = xi-xm;
gammam = xj-xi;
B = [betai 0 betaj 0 betam 0 ; 
   0 gammai 0 gammaj 0 gammam ;
   gammai betai gammaj betaj gammam betam]/(2*A);
if p == 1 
   D = (E/(1-NU*NU))*[1 NU 0 ; NU 1 0 ; 0 0 (1-NU)/2];
elseif p == 2
   D = (E/(1+NU)/(1-2*NU))*[1-NU NU 0 ; NU 1-NU 0 ; 0 0 (1-2*NU)/2];
end
y = t*A*B'*D*B;

