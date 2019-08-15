function fig0510
% Figure 5.10, Van der Pol equation

clf
disp(' Call first DEMO2-1 ')
load datenC1 YY OMGA MU
%MU
YY = [YY, YY(:,1)];
% -- X-Achse -------------
c  = 0.4; d  = 0.2;
X1 = [-4, 4]; Y1 = [0,0];
arrow(X1,Y1,c,d,'k',2), hold on
% -- Y-Achse --------------
c  = 0.4; d  = 0.2;
X1 = [0,0]; Y1 = [-6,6];
arrow(X1,Y1,c,d,'k',2), hold on
K = size(YY,1);
for I = 1:K/2
   I = I-1;
   plot(YY(2*I+1,:),YY(2*I+2,:),'k','linewidth',2), hold on
end
plot(4,6,'w'),hold on
plot(-4,-6,'w')
axis equal tight
grid off
text(2,5,'\mu = 2','fontsize',24)
text(-0.5,-5,'\mu = 0.0625','fontsize',24)
X = [1.8,0.25];
Y = [-4.4,-0.39];
arrow(X,Y,c,d,'k',1);
