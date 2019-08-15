function bild03
% Long Channel cf. Petera/Nassehi
% Exakte Loesung und N"aherung nach 300 Sek.
clf, hold on
load daten3a p e t MESSPKT
load daten3b TIME MONITOR_Z V Parmeter
%MESSPKT
%JJ = [2,2+3*[1:30]]

bild00(p,e,t), pause
%plot(p(1,MESSPKT),p(2,MESSPKT),'k*','markersize',6)
%pause
A = Parmeter(1); PERIOD = Parmeter(2); g = Parmeter(3);
L = Parmeter(6); H = Parmeter(7);
clf, hold on
%LU = [1,-2]; plot(LU(1),LU(2),'w.'), hold on
%RO = [6,4]; plot(RO(1),RO(2),'w.'), hold on
TEND = TIME(end)/100;
XX = [3,TEND]; YY = 0*XX;
plot(XX,YY,'k','linewidth',2), hold on

%axis([3,6,-0.5,0.5]), axis manual

plot(TIME/100,MONITOR_Z,'r','linewidth',2), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
% exakte Loesung, aber H statt H + Z im Nenner
% L = 3000, g = 10, H = 10 
TA = L/sqrt(g*H);
EXACT = A*sin(2*pi*(TIME - TA)/PERIOD);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot(TIME/100,EXACT,'b','linewidth',2), hold on
grid on
clear
