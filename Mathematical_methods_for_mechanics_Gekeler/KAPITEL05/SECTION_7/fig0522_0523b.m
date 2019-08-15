function fig0522_0523b
% Figure 5.22, Example of RHeinboldt

disp(' Call first DEMO2-3 ')
clf
load daten3 WEG1 TANG1 STEP1
plot3(WEG1(1,:),WEG1(2,:),WEG1(3,:),'k','linewidth',2), hold on
plot3(WEG1(1,1),WEG1(2,1),WEG1(3,1),'.','Markersize',6), hold on
N = size(WEG1,2);
plot3(WEG1(1,N),WEG1(2,N),WEG1(3,N),'*','Markersize',6), hold on
TT = linspace(-2,4,40);
%TT = linspace(-2,-1.5,40);

Z1 = -(11/6)*TT.^3 + (2/3)*TT.^2 + 19*TT + (107/3);
Z2 = TT;
Z3 = (1/12)*TT.^3 - (1/6)*TT.^2 - 0.5*TT + (1/3);
plot3(Z1,Z2,Z3,'r','linewidth',1)
grid on
axis tight           % scaled !!!!!!!!!!!!
axis equal tight    % unscaled !!!!!!!!!!  
xlabel('y_1','fontsize',12)
ylabel('y_2','fontsize',12)
zlabel('y_3','fontsize',12)
%STEP1
