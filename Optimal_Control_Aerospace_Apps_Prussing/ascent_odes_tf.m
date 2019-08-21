function dX_dtau = ascent_odes_tf(tau,X,tf)
global g_accel Thrust2Weight
Acc = Thrust2Weight*g_accel;

xdot = X(3);
ydot = X(4);
Vxdot = Acc*(1/sqrt(1+X(6)^2));
Vydot = Acc*(X(6)/sqrt(1+X(6)^2)) - g_accel;
lambda2_bar_dot = 0;
lambda4_bar_dot = -X(5);
dX_dtau = tf*[xdot; ydot; Vxdot; Vydot; lambda2_bar_dot; lambda4_bar_dot];
return
