function bld050607
clf
load datenC4 YY OMGA MU Parmeter
YY = [YY, YY(:,1)];
YY0 = Parmeter(2:4);
YY0 = YY0*ones(1,size(YY,2));
Z = YY(1:3,:) + YY0;
plot(Z(1,:),Z(2,:),'k','linewidth',2), hold on
Z = YY(4:6,:) + YY0;
plot(Z(1,:),Z(2,:),'k','linewidth',2), hold on
Z = YY(7:9,:) + YY0;
plot(Z(1,:),Z(2,:),'k','linewidth',2), hold on
Z = YY(10:12,:) + YY0;
plot(Z(1,:),Z(2,:),'k','linewidth',2), hold on
xlabel('y_1','fontsize',24)
ylabel('y_2','fontsize',24)
zlabel('y_3','fontsize',24)
grid on
axis equal tight
