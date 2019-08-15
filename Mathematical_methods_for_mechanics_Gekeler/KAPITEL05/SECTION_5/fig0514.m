function fig0514
% Figure 5.14, Lorentz equation

disp(' Call first DEMO2-5 ')
clf
load datenC5 YY OMGA MU Parmeter
YY = [YY, YY(:,1)];
YY0 = Parmeter(5:7);
YY0 = YY0*ones(1,size(YY,2));
Z = YY(1:3,:) + YY0;
plot3(Z(1,:),Z(2,:),Z(3,:),'k','linewidth',2), hold on
Z = YY(4:6,:) + YY0;
plot3(Z(1,:),Z(2,:),Z(3,:),'k','linewidth',2), hold on
Z = YY(7:9,:) + YY0;
plot3(Z(1,:),Z(2,:),Z(3,:),'k','linewidth',2), hold on
Z = YY(10:12,:) + YY0;
plot3(Z(1,:),Z(2,:),Z(3,:),'k','linewidth',2), hold on
xlabel('y_1','fontsize',24)
ylabel('y_2','fontsize',24)
zlabel('y_3','fontsize',24)
grid on
axis equal tight
