function Y = bsp05c(V,flag,Parmeter)
% Data for solving bif. equation of BSP05 by Newton's method
% flag = 1: bifurcation equation 
% flag = 2: Gradient_zeta of bifurcation equation
global UU VV W
SIGN = Parmeter(3); 
n  = Parmeter(4); m = Parmeter(9);
Eps = Parmeter(8); cc = Parmeter(10);
LAMBDA = V(1); ZETA = V(2:end); 
X = Eps*UU*ZETA + W;
FALL = 1;
switch FALL
case 1
   TT = linspace(0,pi,n+4); XA = 3*sin(TT);
   XA = XA(2:n+3);
   TT = linspace(0,pi,n+2); XB = 3*sin(TT);
   XB = XB(2:n+1);
   C = XA.'*XB; C = C(:);
case 2
   XA = ones(1,n+2);
   M = floor(n/4); XA(1:M) = 0;
   M = ceil(3*n/4); XA(M:n) = 0;
   C = XA.'*ones(1,n); C = C(:);
end
switch flag
case 1
   Y = [VV*(LAMBDA*X + SIGN*C.*X.^3); ZETA.'*ZETA - 1];
case 2
   Y = [VV*X, Eps*VV*(LAMBDA*UU + 3*SIGN*diag(C.*X.^2)*UU); 0, 2*ZETA.'];
end
