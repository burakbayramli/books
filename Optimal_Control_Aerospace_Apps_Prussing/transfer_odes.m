function dX_dtau = transfer_odes(tau,x)
global eta mdot Tbar tf m0
m = m0-abs(mdot)*tau*tf;

drbar_dtau = x(2)*eta;
dubar_dtau = x(3)^2/x(1)*eta-eta/x(1)^2 ...
-Tbar/m*(x(6)/sqrt(x(6)^2+x(7)^2));
dvbar_dtau = -x(2)*x(3)/x(1)*eta-Tbar/m*(x(7)/sqrt(x(6)^2+x(7)^2));
dtheta_dtau = x(3)/x(1)*eta;
dlambda_r_bar_dtau = x(6)*(x(3)^2/x(1)^2*eta-2*eta/x(1)^3) ...
-x(7)*x(2)*x(3)/x(1)^2*eta;
dlambda_u_bar_dtau = -x(5)*eta+x(7)*x(3)/x(1)*eta;
dlambda_v_bar_dtau = -x(6)*2*x(3)/x(1)*eta+x(7)*x(2)/x(1)*eta;

dX_dtau = [drbar_dtau; dubar_dtau; dvbar_dtau; dtheta_dtau; ...
dlambda_r_bar_dtau; dlambda_u_bar_dtau; dlambda_v_bar_dtau];
return

