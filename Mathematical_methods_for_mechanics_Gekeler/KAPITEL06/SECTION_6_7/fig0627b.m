function fig0627b
% Figure 6.27, Case 2
% -- Plots V_EFF and D_PHI ------------

disp(' Call first demo2, ex. 2 if not done ')
clf
load daten Parmeter theta1 theta2 d3 D3
T1    = Parmeter(1); T3   = Parmeter(2);
gl    = Parmeter(3); m    = Parmeter(4);
delta = 0.2; NN = 500;
TT1       = linspace(delta,pi-delta,NN);
V_EFF    = v_eff(TT1,Parmeter,d3,D3);
J = find(V_EFF < 6.5); TT1 = TT1(J); V_EFF = V_EFF(J);
TT2       = linspace(delta,pi-delta,NN);
D_PHI    = d_phi(TT2,Parmeter,d3,D3);
J = find(D_PHI > -3.5); TT2 = TT2(J); D_PHI = D_PHI(J);
V_THETA1 = v_eff(theta1,Parmeter,d3,D3);
V_THETA2 = v_eff(theta2,Parmeter,d3,D3);
XA = [pi,pi];    YA = [3,6];
XC = [-0.1,3.2]; YC = [V_THETA1,V_THETA1];
XA = [pi,pi];    YA = [2.5,6.5];
XD = [0,0];      YD = [2.5,6.5];
plot(TT1,V_EFF,'k','linewidth',2), hold on
plot(TT2,D_PHI+6,'k:','linewidth',2), hold on
%plot(TT2,D_PHI,'r','linewidth',2), hold on
plot(XC,YC,'k','linewidth',2), hold on
plot(XA,YA,'k--','linewidth',2), hold on
plot(XD,YD,'k--','linewidth',2), hold on
YC = [6,6];
plot(XC,YC,'k:','linewidth',2), hold on
rr = 0.04;
circle(theta1,V_THETA1,rr,'w')
circle(theta2,V_THETA2,rr,'w')
% -- Rand --------------------
plot(-0.1,2.5,'w.','markersize',3), hold on
plot(3.2,6.5,'w.','markersize',3), hold on
text(0.4,3.2,'d\phi/dt+6','fontsize',20)
text(0.8,5.5,'V_{eff}','fontsize',20)
text(0.1,2.7,'0','fontsize',20)
text(2.8,2.7,'\pi','fontsize',20)
text(0.1,6.2,'y = 6','fontsize',16)
text(2.8,4.5,'E','fontsize',20)
grid off
axis equal tight
%axis off
