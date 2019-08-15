function fig0627d
% Figure 6.27, Case 5
% -- Plots V_EFF and D_PHI ------------

disp(' Call first demo2, ex. 5 if not done ')
clf
load daten Parmeter theta1 theta2 d3 D3
T1    = Parmeter(1); T3   = Parmeter(2);
gl    = Parmeter(3); m    = Parmeter(4);
delta = 0.1; NN = 500;
TT1       = linspace(0.05,pi-delta,NN);
V_EFF    = v_eff(TT1,Parmeter,d3,D3);
J = find(V_EFF < 3); TT1 = TT1(J); V_EFF = V_EFF(J);
TT2       = linspace(delta,pi-delta,NN);
D_PHI    = d_phi(TT2,Parmeter,d3,D3);
J = find(D_PHI < 3); TT2 = TT2(J); D_PHI = D_PHI(J);
K = find(D_PHI > -1); TT2 = TT2(K); D_PHI = D_PHI(K);
V_THETA1 = v_eff(theta1,Parmeter,d3,D3);
V_THETA2 = v_eff(theta2,Parmeter,d3,D3);
XC = [-0.1,3.2]; YC = [V_THETA1,V_THETA1];
XA = [pi,pi];    YA = [-1,3];
XD = [0,0];      YD = [-1,3];
plot(TT1,V_EFF,'k','linewidth',2), hold on
plot(TT2,D_PHI,'k:','linewidth',2), hold on
plot(XC,YC,'k','linewidth',2), hold on
plot(XA,YA,'k--','linewidth',2), hold on
plot(XD,YD,'k--','linewidth',2), hold on
rr = 0.04;
circle(theta1,V_THETA1,rr,'w')
circle(theta2,V_THETA2,rr,'w')
% -- Rand --------------------
plot(-0.1,-1,'w.','markersize',3), hold on
plot(3.2,3,'w.','markersize',3), hold on
text(1.1,2,'V_{eff}','fontsize',20)
text(1.6,0.5,'d\phi/dt','fontsize',20)
text(0.1,-0.8,'0','fontsize',20)
text(2.85,-0.8,'\pi','fontsize',20)
text(2.85,1.3,'E','fontsize',20)

grid off
axis equal tight
%axis off
