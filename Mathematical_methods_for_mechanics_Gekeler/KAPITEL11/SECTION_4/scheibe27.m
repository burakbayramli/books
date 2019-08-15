function scheibe27
clf, hold on, clear
scheibe = 100; KK = [1,2];
while ~ismember(scheibe,KK)
   scheibe = input('Welche Scheibe? (1/2)')
end
%scheibe = 2;
switch scheibe
case 1 % kleine Scheibe
   alfa = pi/9; % Eingriffswinkel
   c    = 0.25; % Faktor fuer Kopfspiel
   rw   = 34; % Radius des Waelzkreises [mm]
   m    = 4;  % Modul (Bezugsmass)
   z    = 17; % Zahnzahl
case 2 % grosse Scheibe
   alfa = pi/9; % Eingriffswinkel
   c    = 0.25; % Faktor fuer Kopfspiel
   rw   = 162;
   m    = 4;
   z    = 81;
end
rG = rw*cos(alfa); % Radius Grundkreis
rF = rw - m - c*m; % Radius Fusskreis
rK = rw + m;       % Radius Kopfkreis
xi1 = sqrt((rK^2 - rG^2)/rG^2);
if rF < rG
   % hier ist die Zahnflanke der Einfachheit halber
   % eine Strecke zwischen rF und rG
   % -- Evolvente -----------------
   NN = 20;
   XI1 = linspace(0,tan(alfa),10);
   XI2 = linspace(tan(alfa),xi1,11);
   XI = [XI1,XI2(2:11)]; % tan(alpha) ist Nr 10, gesamt 20
   XXR = [rF,rG*cos(XI) + rG*XI.*sin(XI);
          0 ,rG*sin(XI) - rG*XI.*cos(XI)];
   XXL = [rF,rG*cos(XI) + rG*XI.*sin(XI);
          0 ,-rG*sin(XI) + rG*XI.*cos(XI)];
   % Winkel gegen Mitte am Grundkreis
   beta = m*pi/(4*rw) + tan(alfa) - alfa;
   % Rechte Flanke um beta im Uhrzeigersinn drehen
   % Linke Flanke um beta gegen Uhrzeigersinn drehen
   c = cos(beta); s = sin(beta);
   D = [c, -s; s, c]; DT = D';
   XXR = DT*XXR;    XXL = D*XXL;
   XXL = fliplr(XXL);
end
if rF == rG % rF = rG setzen
   % -- Evolvente -----------------
   NN = 20;
   xi1 = sqrt((rK^2 - rG^2)/rG^2);
   XI1 = linspace(0,tan(alfa),10);
   XI2 = linspace(tan(alfa),xi1,11);
   XI = [XI1,XI2(2:11)]; % tan(alpha) ist Nr 10, gesamt 20
   XXR = [rG*cos(XI) + rG*XI.*sin(XI);
          rG*sin(XI) - rG*XI.*cos(XI)];
   XXL = [rG*cos(XI) + rG*XI.*sin(XI);
          -rG*sin(XI) + rG*XI.*cos(XI)];
   beta = m*pi/(4*rw) + tan(alfa) - alfa;
   c = cos(beta); s = sin(beta);
   D = [c, -s; s, c]; DT = D';
   XXR = DT*XXR; XXL = D*XXL;
   XXL = fliplr(XXL);
end
if rF > rG % Evolvente faengt bei rF an (schoeneres Bild)
   % hier Strecke zwischen rF und rG
   % -- Evolvente -----------------
   NN = 20;
   xi0 = sqrt((rF^2 - rG^2)/rG^2);
   xi1 = sqrt((rK^2 - rG^2)/rG^2);
   XI1 = linspace(xi0,tan(alfa),10);
%   XI1 = linspace(0,tan(alfa),10);

   XI2 = linspace(tan(alfa),xi1,11);
   XI = [XI1,XI2(2:11)]; % tan(alpha) ist Nr 10, gesamt 20
   XXR = [rG*cos(XI) + rG*XI.*sin(XI);
          rG*sin(XI) - rG*XI.*cos(XI)];
   XXL = [rG*cos(XI) + rG*XI.*sin(XI);
          -rG*sin(XI) + rG*XI.*cos(XI)];
   % Winkel gegen Mitte am Grundkreis
   alfaF = acos(rw*cos(alfa)/rF);
   beta = m*pi/(4*rw) + (tan(alfa) - alfa);
   % Rechte Flanke um beta im Uhrzeigersinn drehen
   % Linke Flanke um beta gegen Uhrzeigersinn drehen
   c = cos(beta); s = sin(beta);
   D = [c, -s; s, c]; DT = D';
   XXR = DT*XXR; XXL = D*XXL;
   XXL = fliplr(XXL);
end
% -- Teilung --------------
PP = 2*pi/z; c = cos(PP); s = sin(PP);
D = [c, -s; s, c];
XX = [XXR,XXL];
% -- Zaehne Drehen  --------------
XXA = XX;
for I = 2:z
    c1 = cos((I-1)*PP); s1 = sin((I-1)*PP);
    D = [c1, -s1; s1, c1];
    XX1 = D*XX;
    XXA = [XXA,XX1];
end
XXA = [XXA,XXA(:,1)]; % fuer reine Drehung, sonst Polygon offen
% -- Grafik -------------------------
% -- Bildjustierung -----------------------
qq = 1.2; % Faktor
plot(-qq*rw,-qq*rw,'k.'), hold on
plot(qq*rw,qq*rw,'k.'), hold on
axis equal tight, axis manual, grid on
TT = linspace(0,2*pi,80);
% -- Waelzkreis -----------------------
X2 = rw*cos(TT); Y2 = rw*sin(TT);
plot(X2,Y2,'k','linewidth',2), hold on
% -- Grundkreis --------------
X2 = rG*cos(TT); Y2 = rG*sin(TT);
plot(X2,Y2,'b','linewidth',2), hold on
% -- Kopfkreis --------------
X2 = rK*cos(TT); Y2 = rK*sin(TT);
plot(X2,Y2,'g','linewidth',2), hold on
if rF ~= rG
   % -- Fusskreis --------------
   X2 = rF*cos(TT); Y2 = rF*sin(TT);
   plot(X2,Y2,'r','linewidth',2), hold on
end
%fill(XXA(1,:),XXA(2,:),'y'),hold on
plot(XXA(1,:),XXA(2,:),'b','linewidth',2), hold on
% -- Teilung --------------------
PP = 2*pi/z;
% halbe Zahndicke am Waelzkreis
beta = m*pi/(4*rw);
for I = 1:z
    gama = (I-1)*PP-beta;
    X1 = [cos(gama)*[rF, rK]; sin(gama)*[rF, rK]];
    plot(X1(1,:),X1(2,:),'k'), hold on
end
% Um halbe Zahndicke zurueck-bzw. vordrehen
c = cos(beta); s = sin(beta);
D = [c, -s; s, c]; DT = D';
switch scheibe
case 1, % Scheibe A muss rueckwaertslaufen
    XXA = fliplr(XXA);
    XXA = D*XXA;
case 2, XXA = DT*XXA;
end

RR = rw/20; circle(0,0,RR,'w')
% - die Kontaktpunkte -------
switch scheibe
case 1, plot(XXA(1,367),XXA(2,367),'k*')
case 2, plot(XXA(1,31),XXA(2,31),'k*')
end
switch scheibe
case 1, RRA = rw; save scheibe26 XXA RRA
case 2, RRB = rw; XXB = XXA; save scheibe27 XXB RRB
end
