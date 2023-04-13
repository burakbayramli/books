function w = QuadraticQuadElementStiffness(E,NU,h,x1,y1,x2,y2,x3,y3,x4,y4,p)
%QuadraticQuadElementStiffness   This function returns the element 
%                                stiffness matrix for a quadratic   
%                                quadrilateral element with modulus 
%                                of elasticity E, Poisson's ratio 
%                                NU, thickness h, coordinates of 
%                                node 1 (x1,y1), coordinates 
%                                of node 2 (x2,y2), coordinates of 
%                                node 3 (x3,y3), and coordinates of 
%                                node 4 (x4,y4). Use p = 1 for cases 
%                                of plane stress, and p = 2 for 
%                                cases of plane strain.
%                                The size of the element 
%                                stiffness matrix is 16 x 16.
syms s t;
x5 = (x1 + x2)/2;
x6 = (x2 + x3)/2;
x7 = (x3 + x4)/2;
x8 = (x4 + x1)/2;
y5 = (y1 + y2)/2;
y6 = (y2 + y3)/2;
y7 = (y3 + y4)/2;
y8 = (y4 + y1)/2;
N1 = (1-s)*(1-t)*(-s-t-1)/4;
N2 = (1+s)*(1-t)*(s-t-1)/4;
N3 = (1+s)*(1+t)*(s+t-1)/4;
N4 = (1-s)*(1+t)*(-s+t-1)/4;
N5 = (1-t)*(1+s)*(1-s)/2;
N6 = (1+s)*(1+t)*(1-t)/2;
N7 = (1+t)*(1+s)*(1-s)/2;
N8 = (1-s)*(1+t)*(1-t)/2;
x = N1*x1 + N2*x2 + N3*x3 + N4*x4 + N5*x5 + N6*x6 + N7*x7 + N8*x8;
y = N1*y1 + N2*y2 + N3*y3 + N4*y4 + N5*y5 + N6*y6 + N7*y7 + N8*y8;
xs = diff(x,s);
xt = diff(x,t);
ys = diff(y,s);
yt = diff(y,t);
J = xs*yt - ys*xt;
N1s = diff(N1,s);
N2s = diff(N2,s);
N3s = diff(N3,s);
N4s = diff(N4,s);
N5s = diff(N5,s);
N6s = diff(N6,s);
N7s = diff(N7,s);
N8s = diff(N8,s);
N1t = diff(N1,t);
N2t = diff(N2,t);
N3t = diff(N3,t);
N4t = diff(N4,t);
N5t = diff(N5,t);
N6t = diff(N6,t);
N7t = diff(N7,t);
N8t = diff(N8,t);
B11 = yt*N1s - ys*N1t;
B12 = 0;
B13 = yt*N2s - ys*N2t;
B14 = 0;
B15 = yt*N3s - ys*N3t;
B16 = 0;
B17 = yt*N4s - ys*N4t;
B18 = 0;
B19 = yt*N5s - ys*N5t;
B110 = 0;
B111 = yt*N6s - ys*N6t;
B112 = 0;
B113 = yt*N7s - ys*N7t;
B114 = 0;
B115 = yt*N8s - ys*N8t;
B116 = 0;
B21 = 0;
B22 = xs*N1t - xt*N1s;
B23 = 0;
B24 = xs*N2t - xt*N2s;
B25 = 0;
B26 = xs*N3t - xt*N3s;
B27 = 0;
B28 = xs*N4t - xt*N4s;
B29 = 0;
B210 = xs*N5t - xt*N5s;
B211 = 0;
B212 = xs*N6t - xt*N6s;
B213 = 0;
B214 = xs*N7t - xt*N7s;
B215 = 0;
B216 = xs*N8t - xt*N8s;
B31 = xs*N1t - xt*N1s;
B32 = yt*N1s - ys*N1t;
B33 = xs*N2t - xt*N2s;
B34 = yt*N2s - ys*N2t;
B35 = xs*N3t - xt*N3s;
B36 = yt*N3s - ys*N3t;
B37 = xs*N4t - xt*N4s;
B38 = yt*N4s - ys*N4t;
B39 = xs*N5t - xt*N5s;
B310 = yt*N5s - ys*N5t;
B311 = xs*N6t - xt*N6s;
B312 = yt*N6s - ys*N6t;
B313 = xs*N7t - xt*N7s;
B314 = yt*N7s - ys*N7t;
B315 = xs*N8t - xt*N8s;
B316 = yt*N8s - ys*N8t;
% The expression of B below is not divided by J - it is adjusted for later 
% in the calculation of BD below.
B = [B11 B12 B13 B14 B15 B16 B17 B18 B19 B110 B111 B112 B113 B114 B115 B116;
   B21 B22 B23 B24 B25 B26 B27 B28 B29 B210 B211 B212 B213 B214 B215 B216;
   B31 B32 B33 B34 B35 B36 B37 B38 B39 B310 B311 B312 B313 B314 B315 B316];
if p == 1 
   D = (E/(1-NU*NU))*[1, NU, 0 ; NU, 1, 0 ; 0, 0, (1-NU)/2];
elseif p == 2
   D = (E/(1+NU)/(1-2*NU))*[1-NU, NU, 0 ; NU, 1-NU, 0 ; 0, 0, (1-2*NU)/2];
end
Bnew = simplify(B);
Jnew = simplify(J);
BD = transpose(Bnew)*D*Bnew/Jnew;
r = int(int(BD, t, -1, 1), s, -1, 1);
z = h*r;
w = double(z);


