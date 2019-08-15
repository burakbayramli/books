function fig0512
% Figure 5.12, Small brusselator

clf
disp(' Call first DEMO2-3 ')
load datenC3 YY OMGA MU
%MUE
YY = [YY, YY(:,1)];
% -- X-Achse -------------
c  = 0.4; d  = 0.2;
X1 = [-1, 5]; Y1 = [0,0];
arrow(X1,Y1,c,d,'k',2), hold on
% -- Y-Achse --------------
c  = 0.4; d  = 0.2;
X1 = [0,0]; Y1 = [-4,3];
arrow(X1,Y1,c,d,'k',2), hold on
K = size(YY,1); 
%plot(YY(1,:),YY(2,:),'k','linewidth',2), hold on
for I = 1:K/2
   I = I - 1;
   plot(YY(2*I+1,:),YY(2*I+2,:),'k','linewidth',2), hold on
end
plot(5,3,'w'),hold on
plot(-1,-4,'w')
axis equal tight
grid on
text(1.3,1.5,'\mu = 1.5','fontsize',24)
text(0.5,-3.3,'\mu = 0.13','fontsize',24)
X = [1.2,0.3];
Y = [-3.1,-0.6];
arrow(X,Y,c,d,'k',1);
grid off

