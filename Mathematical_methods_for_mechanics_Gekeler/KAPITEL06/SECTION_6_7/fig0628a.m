function fig0628a
% Figure 6.28, Case 8
% Plots V_EFF and D_PHI ---------------

disp(' Call first demo2, ex. 8 if not done ')
clf
load daten Parmeter theta1 theta2 d3 D3
T1    = Parmeter(1); T3   = Parmeter(2);
gl    = Parmeter(3); m    = Parmeter(4);
delta = 0.1; NN = 500;
TT1       = linspace(0,pi-3*delta,NN);
V_EFF    = v_eff(TT1,Parmeter,d3,D3);
J = find(V_EFF < 8); TT1 = TT1(J); V_EFF = V_EFF(J);
TT2       = linspace(0,pi-delta,NN);
D_PHI    = d_phi(TT2,Parmeter,d3,D3);
J = find(D_PHI < 8); TT2 = TT2(J); D_PHI = D_PHI(J);
%K = find(D_PHI > -0.5); TT2 = TT2(K); D_PHI = D_PHI(K);
V_THETA1 = v_eff(theta1,Parmeter,d3,D3);
V_THETA2 = v_eff(theta2,Parmeter,d3,D3);
XA = [pi,pi];    YA = [0,4];
XC = [-0.1,3.2]; YC = [0.5*V_THETA2,0.5*V_THETA2];
XD = [0,0];      YD = [0,4];
plot(TT1,0.5*V_EFF,'k','linewidth',2), hold on
plot(TT2,0.5*D_PHI,'k:','linewidth',2), hold on
plot(XA,YA,'k--','linewidth',2), hold on
plot(XC,YC,'k','linewidth',2), hold on
plot(XD,YD,'k--','linewidth',2), hold on
rr = 0.04;
circle(theta1,0.5*V_THETA1,rr,'w')
circle(theta2,0.5*V_THETA2,rr,'w')
% -- Rand --------------------
plot(-0.1,0,'w.','markersize',3), hold on
plot(3.2,4,'w.','markersize',3), hold on
text(0.1,2.5,'0.5 V_{eff}','fontsize',20)
text(1.6,1.3,'0.5 d\phi/dt','fontsize',20)
text(0.1,0.2,'0','fontsize',20)
text(2.85,0.2,'\pi','fontsize',20)
text(2.85,3.2,'E','fontsize',20)

grid off
axis equal tight
%axis off
