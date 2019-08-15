function Y = bsp04c(V,flag,Parmeter)
% Data for solving bif. equation of BSP04 by Newton's method
% flag = 1: bifurcation equation 
% flag = 2: Gradient_zeta of bifurcation equation
global UU W
SIGN = Parmeter(3); Eps = Parmeter(8); cc = Parmeter(10);
LAMBDA = V(1); ZETA = V(2:end); 
X = Eps*UU*ZETA + W;
switch flag
case 1
   Y = [UU.'*(LAMBDA*X + sin(X) - X); ZETA.'*ZETA - 1];
case 2
   Y = [UU.'*X, Eps*UU.'*(LAMBDA*UU + diag(cos(X) - 1)*UU); 0, 2*ZETA.'];
end
