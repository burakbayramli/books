function fig0627e
% Figure 6.27, Case 6
% -- Plots V_EFF and D_PHI ------------

disp(' Call first demo2, ex. 6 if not done ')
clf
load daten Parmeter theta1 theta2 d3 D3
T1    = Parmeter(1); T3   = Parmeter(2);
gl    = Parmeter(3); m    = Parmeter(4);
delta = 0.01; NN = 500;
TT1       = linspace(delta,pi-delta,NN);
V_EFF    = v_eff(TT1,Parmeter,d3,D3);
J = find(V_EFF < 4); TT1 = TT1(J); V_EFF = V_EFF(J);
TT2       = linspace(2.1*delta,pi-4.4*delta,NN);
D_PHI    = d_phi(TT2,Parmeter,d3,D3);
J = find(D_PHI < 4); TT2 = TT2(J); D_PHI = D_PHI(J);
K = find(D_PHI > -0.5); TT2 = TT2(K); D_PHI = D_PHI(K);
V_THETA1 = v_eff(theta1,Parmeter,d3,D3);
V_THETA2 = v_eff(theta2,Parmeter,d3,D3);
XC = [-0.1,3.2]; YC = [V_THETA1,V_THETA1];
XA = [pi,pi];    YA = [0,4];
XD = [0,0];      YD = [0,4];
plot(TT1,V_EFF,'k','linewidth',2), hold on
plot(TT2,D_PHI,'k:','linewidth',2), hold on
plot(XC,YC,'k','linewidth',2), hold on
plot(XA,YA,'k--','linewidth',2), hold on
plot(XD,YD,'k--','linewidth',2), hold on
rr = 0.04;
circle(theta1,V_THETA1,rr,'w')
circle(theta2,V_THETA2,rr,'w')
% -- Rand --------------------
plot(-0.1,2.5,'w.','markersize',3), hold on
plot(3.2,3,'w.','markersize',3), hold on
text(1.95,1.2,'V_{eff}','fontsize',20)
text(0.5,2,'d\phi/dt','fontsize',20)
text(0.1,0.2,'0','fontsize',20)
text(2.85,0.2,'\pi','fontsize',20)
text(2.85,0.95,'E','fontsize',20)

grid off
axis equal tight
%axis off
