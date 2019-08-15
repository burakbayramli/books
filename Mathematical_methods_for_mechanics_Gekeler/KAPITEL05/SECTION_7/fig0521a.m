function fig0521a
% Figure 5.21, Example of Kubicek

disp('Call first DEMO1-2 ')
clf
load daten2 WEG
plot(WEG(5,:),WEG(2,:),'k','linewidth',2), hold on
%plot(WEG(5,:),WEG(2,:),'.','markersize',6), hold on

plot(WEG(5,1),WEG(2,1),'.','Markersize',12), hold on
N = size(WEG,2);
plot(WEG(5,N),WEG(2,N),'*','Markersize',6)
grid on
%axis equal tight
%xlabel('y_1')
%ylabel('y_2')
%zlabel('y_3')
