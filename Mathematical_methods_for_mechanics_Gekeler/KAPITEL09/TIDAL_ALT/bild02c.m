function bild02c
% Image for shallow water in a Channel
%clc
%   Parmeter = [DT,A,H,L,PERIOD,g,N,TAU,XSCALE,YSCALE,NN];
load daten5a p e t Parmeter Example HOURS
HOURS
Example = 4;
switch Example
   case 1, load daten5b MONITOR_U MONITOR_Z 
   case 2, load daten5c MONITOR_U MONITOR_Z 
   case 3, load daten5d MONITOR_U MONITOR_Z 
   case 4, load daten5e MONITOR_U MONITOR_V MONITOR_Z

end
disp(' Plot Z at left and right channel end ')
clf, hold on
MAXWERT = max(max(abs(MONITOR_Z(:,7))),max(abs(MONITOR_Z(:,14))));
axis([0 size(MONITOR_Z,1) -MAXWERT MAXWERT])
axis manual, grid on
plot(MONITOR_Z(:,7),'r'), hold on
plot(MONITOR_Z(:,14),'b'), hold on

