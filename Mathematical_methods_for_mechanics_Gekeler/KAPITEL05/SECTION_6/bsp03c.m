function Y = bsp03c(V,flag,Parmeter)
% Data for solving bif. equation of BSP03 by Newton's method
% flag = 1: bifurcation equation 
% flag = 2: Gradient_zeta of bifurcation equation
global UU VV W
SIGN = Parmeter(3); Eps = Parmeter(8); cc = Parmeter(10);
LAMBDA = V(1); ZETA = V(2:end); 
X = Eps*UU*ZETA + W;
switch flag
case 1
   Y = [VV*(LAMBDA*X + cc*SIGN*X.^3); ZETA.'*ZETA - 1];
case 2
   Y = [VV*X, Eps*VV*(LAMBDA*UU + cc*3*SIGN*diag(X.^2)*UU); 0, 2*ZETA.'];
end
