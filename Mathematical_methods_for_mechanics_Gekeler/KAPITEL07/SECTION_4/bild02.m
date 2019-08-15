function bild02;
% Zeichnet ebenes Stabwerk
% mit Lager (o) und Lasten (*)

load daten2 Z p e LAGER LASTEN PARMETER nr
PHI = PARMETER(4);
MOVIE = moviein(2);
clf
if nr == 1
   axis([-0.5 3.5 -0.5 2.5])
end
if nr == 2
   axis([-1 7 -0.5 1.5])
end
grid on
hold on
H = plot(p(1,:),p(2,:),'.','MarkerSize',6);
for I = 1:size(e,2)
   X = [p(1,e(1,I)),p(1,e(2,I))];
   Y = [p(2,e(1,I)),p(2,e(2,I))];
   H = plot(X,Y,'k','linewidth',2);
   hold on
end
X = p(1,LAGER(1,:));
Y = p(2,LAGER(1,:));
H = plot(p(1,LAGER(1,:)),p(2,LAGER(1,:)),'o','markersize',8);
hold on
AUX = abs(LASTEN(1,:)) + abs(LASTEN(2,:));
J = find(AUX ~= 0);
H = plot(p(1,J),p(2,J),'*','markersize',8);
hold on
% -- Zeichne Strecke durch Knoten 5 ----------
X5 = [-0.5*cos(PHI),0.5*cos(PHI)];
Y5 = [2-0.5*sin(PHI),2+0.5*sin(PHI)];
plot(X5,Y5,'r','linewidth',2)
pause(2)
MOVIE(:,1) = getframe;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set(gca,'NextPlot','replacechildren');
K = newplot;
grid on
hold on
FAKTOR = 50;    % !!!!!!!!!!!!!!!!!!!
p = p + FAKTOR*Z';
%axis equal
hold on
K = plot(p(1,:),p(2,:),'.','MarkerSize',6);
for I = 1:size(e,2)
   X = [p(1,e(1,I)),p(1,e(2,I))];
   Y = [p(2,e(1,I)),p(2,e(2,I))];
   K = plot(X,Y,'k','linewidth',2);
   hold on
end
X = p(1,LAGER(1,:));
Y = p(2,LAGER(1,:));
K = plot(p(1,LAGER(1,:)),p(2,LAGER(1,:)),'o','markersize',8);
hold on
AUX = abs(LASTEN(1,:)) + abs(LASTEN(2,:));
J = find(AUX ~= 0);
K = plot(p(1,J),p(2,J),'*','markersize',8);
hold on
% -- Zeichne Strecke durch Knoten 5 ----------
X5 = [-0.5*cos(PHI),0.5*cos(PHI)];
Y5 = [2-0.5*sin(PHI),2+0.5*sin(PHI)];
plot(X5,Y5,'r','linewidth',2)
pause(2)
MOVIE(:,2) = getframe;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
movie(MOVIE,6,1);
clear
