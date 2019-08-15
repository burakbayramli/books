function bild02b
% Image for shallow water in a Channel
%clc
load daten2a p e t Example HOURS
HOURS
switch Example
   case 1, load daten2b MONITOR_U MONITOR_Z DT
   case 2, load daten2c MONITOR_U MONITOR_Z DT
   case 3, load daten2d MONITOR_U MONITOR_Z DT
   case 4, load daten2e MONITOR_U MONITOR_Z DT
end
disp(' Plot Z at left and right channel end ')
clf, hold on
MAXWERT = max(max(abs(MONITOR_Z(:,7))),max(abs(MONITOR_Z(:,14))));
axis([0 DT*size(MONITOR_Z,1)/3600 -MAXWERT MAXWERT])
axis manual, grid on
TT = DT*[1:size(MONITOR_Z,1)] - DT; TT = TT/3600;
plot(TT,MONITOR_Z(:,7),'r'), hold on
plot(TT,MONITOR_Z(:,14),'b'), hold on

plot([0,TT(end)],[0,0],'k'), hold on
%plot(TT,0*zeros(1,length(TT)),'k.','markersize',12), hold on

%lynch



