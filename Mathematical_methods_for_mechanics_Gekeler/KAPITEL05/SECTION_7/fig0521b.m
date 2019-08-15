function fig0521b
% Figure 5.21, Example of Kubicek

disp(' Call first DEMO2-2 ')
clf
load daten2 WEG1 TANG1 STEP1
plot(WEG1(5,:),WEG1(2,:)), hold on
plot(WEG1(5,:),WEG1(2,:),'.','markersize',6), hold on

plot(WEG1(5,1),WEG1(2,1),'.','Markersize',12), hold on
N = size(WEG1,2);
plot(WEG1(5,N),WEG1(2,N),'*','Markersize',6)
grid on
