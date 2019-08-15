function bld060803
% Sieben-Koerper-Problem, Kurven der 7 Winkel
% disp(' Zuerst DEMO1.M, Beispiel 2  Aufrufen! ')

clf
load daten2 tt uout vout wout
XX = uout;
J1 = find(XX(1,:) < pi);
TT1 = tt(J1); XX1 = XX(1,J1);
J2 = find(XX(1,:) >= pi & XX(1,:) < 3*pi);
TT2 = tt(J2); XX2 = XX(1,J2)-2*pi;
J3 = find(XX(1,:) >= 3*pi & XX(1,:) < 5*pi);
TT3 = tt(J3); XX3 = XX(1,J3)-4*pi;
%plot(TT3,XX3,'b','linewidth',2), hold on
J4 = find(XX(1,:) >= 5*pi & XX(1,:) < 7*pi);
TT4 = tt(J4); XX4 = XX(1,J4)-6*pi;
plot(TT1,XX1,'b',TT2,XX2,'b',TT3,XX3,'b',TT4,XX4,'b',...
     'linewidth',2), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
J1 = find(XX(2,:) > - pi);
TT1 = tt(J1); XX1 = XX(2,J1);
J2 = find(XX(2,:) <= -pi & XX(2,:) > -3*pi);
TT2 = tt(J2); XX2 = XX(2,J2)+2*pi;
J3 = find(XX(2,:) <= -3*pi & XX(2,:) > -5*pi);
TT3 = tt(J3); XX3 = XX(2,J3)+4*pi;
J4 = find(XX(2,:) <= -5*pi & XX(2,:) > - 7*pi);
TT4 = tt(J4); XX4 = XX(2,J4)+6*pi;
plot(TT1,XX1,'r',TT2,XX2,'r',TT3,XX3,'r',TT4,XX4,'r',...
     'linewidth',2), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot(tt,XX(3,:),'k--','linewidth',2), hold on
plot(tt,XX(4,:),'k:','linewidth',2), hold on
plot(tt,XX(5,:),'k-.','linewidth',2), hold on
plot(tt,XX(6,:),'k','linewidth',2), hold on
plot(tt,XX(7,:),'g','linewidth',2), hold on
%legend('\beta','\gamma',3)
text(0.008,1.7,'\beta','fontsize',20)
text(0.0185,1.7,'\beta','fontsize',20)
text(0.027,1.7,'\beta','fontsize',20)
text(0.008,-1.7,'\Theta','fontsize',20)
text(0.0185,-1.7,'\Theta','fontsize',20)
text(0.027,-1.7,'\Theta','fontsize',20)

text(0.021,-0.8,'\gamma','fontsize',20)
text(0.0105,-0.75,'\Phi','fontsize',20)
text(0.002,0.7,'\delta','fontsize',20)
text(0.001,-0.4,'\Omega','fontsize',20)
text(0.002,1.4,'\epsilon','fontsize',20)
c = 0.08; d = 0.0002;
XA = [0.0215;0.0215];
YA = [-0.7;0];
arrow(XA,YA,c,d,'k',1)
%axis([0 0.03 -2 2])
axis([0 0.03 -2 2])

grid off
%axis off
