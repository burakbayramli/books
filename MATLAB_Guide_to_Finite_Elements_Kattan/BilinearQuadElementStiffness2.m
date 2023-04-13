function w = BilinearQuadElementStiffness2(E,NU,h,x1,y1,x2,y2,x3,y3,x4,y4,p)
%BilinearQuadElementStiffness   This function returns the element 
%                               stiffness matrix for a bilinear   
%                               quadrilateral element with modulus 
%                               of elasticity E, Poisson's ratio 
%                               NU, thickness h, coordinates of 
%                               node 1 (x1,y1), coordinates 
%                               of node 2 (x2,y2), coordinates of 
%                               node 3 (x3,y3), and coordinates of 
%                               node 4 (x4,y4). Use p = 1 for cases 
%                               of plane stress, and p = 2 for 
%                               cases of plane strain.
%                               The size of the element 
%                               stiffness matrix is 8 x 8.
syms s t;
N1 = (1-s)*(1-t)/4;
N2 = (1+s)*(1-t)/4;
N3 = (1+s)*(1+t)/4;
N4 = (1-s)*(1+t)/4;
x = N1*x1 + N2*x2 + N3*x3 + N4*x4;
y = N1*y1 + N2*y2 + N3*y3 + N4*y4;
xs = diff(x,s);
xt = diff(x,t);
ys = diff(y,s);
yt = diff(y,t);
J = xs*yt - ys*xt;
N1s = diff(N1,s);
N2s = diff(N2,s);
N3s = diff(N3,s);
N4s = diff(N4,s);
N1t = diff(N1,t);
N2t = diff(N2,t);
N3t = diff(N3,t);
N4t = diff(N4,t);
B11 = yt*N1s - ys*N1t;
B12 = 0;
B13 = yt*N2s - ys*N2t;
B14 = 0;
B15 = yt*N3s - ys*N3t;
B16 = 0;
B17 = yt*N4s - ys*N4t;
B18 = 0;
B21 = 0;
B22 = xs*N1t - xt*N1s;
B23 = 0;
B24 = xs*N2t - xt*N2s;
B25 = 0;
B26 = xs*N3t - xt*N3s;
B27 = 0;
B28 = xs*N4t - xt*N4s;
B31 = xs*N1t - xt*N1s;
B32 = yt*N1s - ys*N1t;
B33 = xs*N2t - xt*N2s;
B34 = yt*N2s - ys*N2t;
B35 = xs*N3t - xt*N3s;
B36 = yt*N3s - ys*N3t;
B37 = xs*N4t - xt*N4s;
B38 = yt*N4s - ys*N4t;
B = [B11 B12 B13 B14 B15 B16 B17 B18 ;
   B21 B22 B23 B24 B25 B26 B27 B28 ;
   B31 B32 B33 B34 B35 B36 B37 B38];
if p == 1 
   D = (E/(1-NU*NU))*[1, NU, 0 ; NU, 1, 0 ; 0, 0, (1-NU)/2];
elseif p == 2
   D = (E/(1+NU)/(1-2*NU))*[1-NU, NU, 0 ; NU, 1-NU, 0 ; 0, 0, (1-2*NU)/2];
end
BD = transpose(B)*D*B/J;
r = int(int(BD, t, -1, 1), s, -1, 1);
z = h*r;
w = double(z);


