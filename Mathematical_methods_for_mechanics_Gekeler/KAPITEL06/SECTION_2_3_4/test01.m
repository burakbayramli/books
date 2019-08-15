function test01
% plots an ellipse
clc, clf
circle(0,0,0.02,'k')
TT = linspace(0,2*pi,80);
e = 0.8; p = 1;
% ----------------------------
PHI = 0.2;
RR = p./(1 - e*cos(TT + PHI));
[XA,YA] = pol2cart(TT,RR);
plot(XA,YA,'k','linewidth',2), hold on

PHI = 0.4;
RR = p./(1 - e*cos(TT + PHI));
[XA,YA] = pol2cart(TT,RR);
%plot(XA,YA,'k--','linewidth',2), hold on

% -- corners of figure -----------
circle(-1,-1.5,0.02,'w')
circle(2.5,1.5,0.02,'w')
axis equal tight
grid on
axis off
