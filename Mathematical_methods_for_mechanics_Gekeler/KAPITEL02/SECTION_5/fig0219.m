function fig0219
% Figure 2.19
% call first DEMO1.M
load daten MZP X
clf
c = 0.06; d = 0.02;
% -- X-axis -----------------
X1 = [-0.2,1.2]; Y1 = [0,0];
arrow(X1,Y1,c,d,'k',2), hold on
% -- Y-axis ------------------
X1 = [0,0]; Y1 = [-0.2,1.2];
arrow(X1,Y1,c,d,'k',2), hold on
% -- Straigh connection ----------
plot(MZP,MZP,'k','linewidth',2), hold on
% -- Solution --------------------
plot(MZP,X(1,:),'k','linewidth',2), hold on
N = length(MZP);
r = 0.011;
for I = 1:N
   circle(MZP(I),X(1,I),r,'w')
   circle(MZP(I),MZP(I),r,'w')
end
axis equal tight, grid on
text(1.1,-0.1,'t','fontsize',22)
text(0.9,0.3,'x_1(t)','fontsize',22)
grid off
