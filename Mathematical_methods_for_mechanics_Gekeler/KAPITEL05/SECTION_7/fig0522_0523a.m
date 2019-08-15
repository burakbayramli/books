function fig0522_0523
% Figure 5.22 and 5.23,  Example of Rheinboldt scaled and unscaled

disp('Call first DEMO1-3 ')
clf
load daten3 WEG
plot3(WEG(1,:),WEG(2,:),WEG(3,:),'k','linewidth',2), hold on
plot3(WEG(1,1),WEG(2,1),WEG(3,1),'.','Markersize',6), hold on
N = size(WEG,2);
plot3(WEG(1,N),WEG(2,N),WEG(3,N),'*','Markersize',6), hold on
TT = linspace(-2,4,40);
Z1 = -(11/6)*TT.^3 + (2/3)*TT.^2 + 19*TT + (107/3);
Z2 = TT;
Z3 = (1/12)*TT.^3 - (1/6)*TT.^2 - 0.5*TT + (1/3);
plot3(Z1,Z2,Z3,'k','linewidth',2)
grid on
axis tight                  % scaled    !!!!!!!!!!!!!!!!!
%axis equal tight           % unscaled  !!!!!!!!!!!!!!!!!
xlabel('y_1','fontsize',22)
ylabel('y_2','fontsize',22)
zlabel('y_3','fontsize',22)
