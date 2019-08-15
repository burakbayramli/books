function bild03
% Long Channel cf. Petera/Nassehi
% Exakte Loesung und N"aherung nach 300 Sek.
clf, hold on
load daten3a p e t MESSPKT
load daten3b TIME MONITOR_Z V Parmeter
bild00(p,e,t)
pause
A = Parmeter(1); PERIOD = Parmeter(2); g = Parmeter(3);
clf, hold on
%LU = [1,-2]; plot(LU(1),LU(2),'w.'), hold on
%RO = [6,4]; plot(RO(1),RO(2),'w.'), hold on
XX = [3,6]; YY = 0*XX;
plot(XX,YY,'k','linewidth',2), hold on
axis([3,6,-0.5,0.5]), axis manual
plot(TIME/100,MONITOR_Z,'r','linewidth',2), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
% exakte LOesung
%AUX = sqrt( g*(p(3,MESSPKT) + V(3,MESSPKT)) );
%AUX = 3.E3/AUX;
%EXACT = A*sin(2*pi*(TIME - AUX)/PERIOD);
%plot(TIME/100,EXACT,'k'), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% exakte Loesung, aber H statt H + Z im Nenner
% x = 3000, g = 10, H = 10 ergibt x/sqrt(g H) = 300
EXACT = A*sin(2*pi*(TIME - 300)/PERIOD);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot(TIME/100,EXACT,'b','linewidth',2), hold on
text(5.5,-0.44, '\times 100 sec.','fontsize',20)
text(3.1,0.44,'m','fontsize',20)
clear
