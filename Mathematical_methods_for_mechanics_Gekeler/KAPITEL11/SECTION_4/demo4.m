function demo4
% Abrollen zweier Zahnraeder mit festen Achsen
% und VORGEBENEN DREHZAHLEN
% Zuerst scheibe27.m Aufrufen!!
clc, format compact, format short g
% -- ALLGEMEINE PARAMETER ---------------------
nr = 1;
switch nr
case 1
   % kleine Scheibe -------------------
   load scheibe26 XXA RRA, % kleine Scheibe
   load scheibe27 XXB RRB, % grosse Scheibe
   u = 4.765; % Uebersetzungsverhaeltnis
   STEPNR = 28;
   % -- Verschiebung ----------------------
   M1 = [RRA;0]; M2 = [-RRB;0];
   % -- Bildjustierung -----------------------
   clf, hold on
   plot(-40,-40,'w.'), hold on
   plot(40,40,'w.'), hold on
end
axis equal tight, axis manual, grid on
NN = 720; PHI = 2*pi/NN; phi = u*2*pi/NN;
c1 = cos(PHI); s1 = sin(PHI);
c2 = cos(phi); s2 = sin(phi);
D1 = [c1, -s1; s1, c1];
D2 = [c2, s2; -s2, c2];
X = XXA + M1*ones(1,size(XXA,2));
Y = XXB + M2*ones(1,size(XXB,2));
plot(X(1,:),X(2,:),'r','linewidth',2), hold on
plot(Y(1,:),Y(2,:),'k','linewidth',2), hold on
fill(X(1,:),X(2,:),'y'), hold on
fill(Y(1,:),Y(2,:),'y'), hold on
FILM(1) = getframe;
set(gca,'nextplot','replacechildren');
for I = 2:STEPNR
   XXA = D2*XXA;
   XXB = D1*XXB;
   X = XXA + M1*ones(1,size(XXA,2));
   Y = XXB + M2*ones(1,size(XXB,2));
   plot(X(1,:),X(2,:),'r','linewidth',2), hold on
   plot(Y(1,:),Y(2,:),'k','linewidth',2), hold on
   fill(X(1,:),X(2,:),'y'), hold on
   fill(Y(1,:),Y(2,:),'y'), hold on
   FILM(I) = getframe;
   set(gca,'nextplot','replacechildren');
end
save datenfilm FILM
axis off
