function w = QuadTriangleElementStiffness(E,NU,t,x1,y1,x2,y2,x3,y3,p)
%QuadTriangleElementStiffness   This function returns the element 
%                                    stiffness matrix for a quadratic   
%                                    triangular element with modulus 
%                                    of elasticity E, Poisson's ratio 
%                                    NU, thickness t, coordinates of 
%                                    the node 1 (x1,y1), coordinates 
%                                    of node 2 (x2,y2), and 
%                                    coordinates of node 3 
%                                    (x3,y3). Use p = 1 for cases of 
%                                    plane stress, and p = 2 for 
%                                    cases of plane strain.
%                                    The size of the element 
%                                    stiffness matrix is 12 x 12.
syms x y;
x4 = (x1 + x2)/2;
y4 = (y1 + y2)/2;
x5 = (x2 + x3)/2;
y5 = (y2 + y3)/2;
x6 = (x1 + x3)/2;
y6 = (y1 + y3)/2;
x21 = x2 - x1;
y21 = y2 - y1;
x23 = x2 - x3;
y23 = y2 - y3;
x46 = x4 - x6;
y46 = y4 - y6;
x13 = x1 - x3;
y13 = y1 - y3;
x16 = x1 - x6;
y16 = y1 - y6;
x31 = x3 - x1;
y31 = y3 - y1;
x54 = x5 - x4;
y54 = y5 - y4;
x24 = x2 - x4;
y24 = y2 - y4;
x56 = x5 - x6;
y56 = y5 - y6;
x36 = x3 - x6;
y36 = y3 - y6;
x41 = x4 - x1;
y41 = y4 - y1;
x43 = x4 - x3;
y43 = y4 - y3;
x51 = x5 - x1;
y51 = y5 - y1;
x61 = x6 - x1;
y61 = y6 - y1;
x63 = x6 - x3;
y63 = y6 - y3;
N1 = (x23*(y-y3)-y23*(x-x3))*(x46*(y-y6)-y46*(x-x6))/((x23*y13-y23*x13)*(x46*y16-y46*x16));
N2 = (x31*(y-y1)-y31*(x-x1))*(x54*(y-y4)-y54*(x-x4))/((x31*y21-y31*x21)*(x54*y24-y54*x24));
N3 = (x21*(y-y1)-y21*(x-x1))*(x56*(y-y6)-y56*(x-x6))/((x21*y31-y21*x31)*(x56*y36-y56*x36));
N4 = (x31*(y-y1)-y31*(x-x1))*(x23*(y-y3)-y23*(x-x3))/((x31*y41-y31*x41)*(x23*y43-y23*x43));
N5 = (x31*(y-y1)-y31*(x-x1))*(x21*(y-y1)-y21*(x-x1))/((x31*y51-y31*x51)*(x21*y51-y21*x51));
N6 = (x21*(y-y1)-y21*(x-x1))*(x23*(y-y3)-y23*(x-x3))/((x21*y61-y21*x61)*(x23*y63-y23*x63));
N1x = diff(N1,x);
N1y = diff(N1,y);
N2x = diff(N2,x);
N2y = diff(N2,y);
N3x = diff(N3,x);
N3y = diff(N3,y);
N4x = diff(N4,x);
N4y = diff(N4,y);
N5x = diff(N5,x);
N5y = diff(N5,y);
N6x = diff(N6,x);
N6y = diff(N6,y);
B = [N1x, 0, N2x, 0, N3x, 0, N4x, 0, N5x, 0, N6x, 0 ;
   0, N1y, 0, N2y, 0, N3y, 0, N4y, 0, N5y, 0, N6y;
   N1y, N1x, N2y, N2x, N3y, N3x, N4y, N4x, N5y, N5x, N6y, N6x];
if p == 1 
   D = (E/(1-NU*NU))*[1, NU, 0 ; NU, 1, 0 ; 0, 0, (1-NU)/2];
elseif p == 2
   D = (E/(1+NU)/(1-2*NU))*[1-NU, NU, 0 ; NU, 1-NU, 0 ; 0, 0, (1-2*NU)/2];
end
BD = transpose(B)*D*B;
l1 = y1 + (x-x1)*(y2-y1)/(x2-x1);
l2 = y1 + (x-x1)*(y3-y1)/(x3-x1);
l3 = y2 + (x-x2)*(y3-y2)/(x3-x2);
r1 = int(int(BD, y, l1, l2), x, x1, x3);
r2 = int(int(BD, y, l1, l3), x, x3, x2);
z = t*(r1+r2);
w = double(z);


