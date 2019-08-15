function bild04
% Long wave on a beach with full mass matrix
% cf. J.Petera/V.Nassehi: Int. J. Numer. Meth. Eng. 39, 4159-4182

load daten4a p e t
load daten4c MONITOR_Z MONITOR_U
% -- Eckpunkte
clf, hold on
plot(0,-0.2,'w.'), hold on
plot(4,0.2,'w.'), hold on
axis tight, axis manual, grid on
MM = [1:40];
MONITOR_T = [2,2+3*MM];
XX = p(1,MONITOR_T);
for I = 1:size(MONITOR_Z,1)
   plot(XX/10,MONITOR_Z(I,:),'b'), hold on
   plot(XX/10,MONITOR_U(I,:),'k'), hold on
end
text(3.5,-0.17, '\times 10 m','fontsize',20)
text(0.14,0.18,'m','fontsize',20)
text(3,0.12,'\zeta','fontsize',20)
text(3,-0.12,'u','fontsize',20)

clear
