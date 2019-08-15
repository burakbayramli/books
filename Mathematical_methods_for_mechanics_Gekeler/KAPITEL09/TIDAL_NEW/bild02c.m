function bild02c
% Image for shallow water in a Channel
% same as bild02b but for bsp03.m
%clc
load daten2a1 p e t Example HOURS
HOURS
switch Example
   case 1, load daten2b1 MONITOR_U MONITOR_Z DT
   case 2, load daten2c1 MONITOR_U MONITOR_Z DT
   case 3, load daten2d1 MONITOR_U MONITOR_Z DT
   case 4, load daten2e1 MONITOR_U MONITOR_Z DT
end
disp(' Plot Z at left and right channel end ')
clf, hold on
MAXWERT = max(max(abs(MONITOR_Z(:,2))),max(abs(MONITOR_Z(:,92))));
axis([0 DT*size(MONITOR_Z,1)/3600 -MAXWERT MAXWERT])
axis manual, grid on
TT = DT*[1:size(MONITOR_Z,1)] - DT; TT = TT/3600;
plot(TT,MONITOR_Z(:,92),'r'), hold on
plot(TT,MONITOR_Z(:,2),'b'), hold on

plot([0,TT(end)],[0,0],'k'), hold on
%plot(TT,0*zeros(1,length(TT)),'k.','markersize',12), hold on

%lynch



