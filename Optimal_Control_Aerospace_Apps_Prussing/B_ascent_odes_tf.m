function dX_dtau = B_ascent_odes_tf(tau,X,tf)

global g_accel Vc h eta beta f m0 mdot

m = m0-abs(mdot)*tau*tf;

xbardot = X(3)*Vc/h; ybardot = X(4)*Vc/h; Vxbardot =
(f/Vc*(-X(6)/sqrt(X(6)^2+X(7)^2)) ...
-eta*exp(-X(2)*beta)*X(3)*sqrt(X(3)^2+X(4)^2)*Vc)/m;
Vybardot = (f/Vc*(-X(7)/sqrt(X(6)^2+X(7)^2)) ...
-eta*exp(-X(2)*beta)*X(4)*sqrt(X(3)^2+X(4)^2)*Vc)/m-g_accel/Vc;
if sqrt(X(3)^2+X(4)^2) == 0
lambda_2_bar = 0;
lambda_3_bar = 0;
lambda_4_bar = -X(5)*Vc/h;
else
lambda_2_bar = ...
-(X(6)*X(3)+X(7)*X(4))*eta*beta*exp(-X(2)*beta)*sqrt(X(3)^2+X(4)^2)*Vc/m;
lambda_3_bar = eta*exp(-X(2)*beta)*Vc*(X(6)*(2*X(3)^2+X(4)^2) ...
+ X(7)*X(3)*X(4))/sqrt(X(3)^2+X(4)^2)/m;
lambda_4_bar = -X(5)*Vc/h+eta*exp(-X(2)*beta)*Vc*(X(7)*(X(3)^2 ...
+2*X(4)^2) ...
+X(6)*X(3)*X(4))/sqrt(X(3)^2+X(4)^2)/m;
end


  












