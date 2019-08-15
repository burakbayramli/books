function test01
% Testen von Geometrie und Randbedingungen
% Bild 1: Offset-Punkte
% Bild 2: Normalen
% Bild 3: Normalen und Nachbarn
% Bild 4: Randelemente
% Bild 5: Randknoten
% Bild 6: Stellen der Randwerte fuer Z
% Bild 7: Stellen der Randwerte fuer W
% Bild 8: Stellen der Randwerte fuer WBOUND

clc, format compact
% -- Das Beispiel --------------------------
ecode = 0; beispnr = 100; KK = [1,2,3,4,5];
%while ~ismember(beispnr,KK)
%   beispnr = input(' Welches Beispiel?, (1/2/3/4/5) ');
%end;
%FAKTOR = 1; % Scaling for normals in image
beispnr = 1
switch beispnr
case 1, disp(' Geometrie unit square')
   FF2 = 'bsp03gb'; FF3 = 'bsp03h2';
   VS     = 1;
   REFINE = 4; % Anzahl Gitterverfeinerungen
   SEGNR  = [1,2,3,4];  % Seg.-nrn. Aussenrand  geordnet
   SEGNR1 = [];         % Seg.-nrn. Innenerand
end
Parmeter = [0,0,VS,0];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clc, ecode = 0; bilda = 100; KK = [0,1,2,3,4,5,6,7,8];
while ~ismember(bilda,KK)
   bilda = input(' Welches Bild?, (0/1/2/3/4/5/6/7/8) ');
end;
[p,e,t,E] = prepar(FF2,REFINE,[SEGNR,SEGNR1]);
% -- Computation of offset data
[RDKN,RDEL,OFFSET,MAXL] = mesh40(p,e,SEGNR,t);
LAENGE = 1.2*MAXL;
[NACHBAR,NORMALEN,ecode] = mesh43(p,e,SEGNR,OFFSET,LAENGE);
if ~isempty(SEGNR1) 
   [RDKN,RDEL,OFFSET,MAXL] = mesh40(p,e,SEGNR1,t);
   LAENGE = 1.2*MAXL;
   [NACHBAR1,NORMALEN1,ecode] = mesh43(p,e,SEGNR1,OFFSET,LAENGE);
   NACHBAR = [NACHBAR,NACHBAR1];
   NORMALEN = [NORMALEN,NORMALEN1];
end
% -- boundary data ------------------------
[RDZ,RDW] = feval(FF3,p,e,t,Parmeter);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clf
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
t1 = t(1:3,:);
hold on  % fuer flaches Bild --------
trimesh(t1',X,Y,Z1,'edgecolor','b'), hold on
axis equal, axis manual, grid on
for I = 1:size(e,2)
   A = [p(1,e(1,I));p(1,e(2,I))];
   B = [p(2,e(1,I));p(2,e(2,I))];
   plot(A,B,'r','linewidth',1), hold on
end
% -- Randsegmente ---------------
for I = 1:size(E,2)
   A = [p(1,E(1,I));p(1,E(2,I))];
   B = [p(2,E(1,I));p(2,E(2,I))];
   plot(A,B,'k','linewidth',2), hold on
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Testen der einzelnen Hilfsprogramme ----
switch bilda
case 1, disp(' Offset-Punkte ')
   KK = 1;
   for I = 1:length(OFFSET)
      XA = p(1,OFFSET(I)); YA = p(2,OFFSET(I));
      if KK == 1, plot(XA,YA,'k*'), hold on, end
      if KK == 0, plot(XA,YA,'ko'), hold on, end
      if KK == 1, KK = 0; else KK = 1; end
      pause(0.2)
   end
case 2, disp(' Normalen ')
   NORMALEN = LAENGE*NORMALEN;
   for I = 1:size(NORMALEN,2)
      XA = p(1,E(1,I)); YA = p(2,E(1,I));
      plot(XA,YA,'k*'), hold on
      XB = [XA; XA + NORMALEN(2,I)]; YB = [YA; YA + NORMALEN(3,I)];
      plot(XB,YB,'r','linewidth',1), hold on
      pause(0.2)
   end
case 3, disp(' Normalen und Nachbarn ')
   NN = size(NACHBAR,2);
   NORMNEU = zeros(2,NN);
   for I = 1:size(NACHBAR,2)
      NORMNEU(:,I) = NACHBAR(4,I)*NORMALEN(2:3,I);
   end
   for I = 1:NN
      XA = p(1,E(1,I)); YA = p(2,E(1,I));
      plot(XA,YA,'k*'), hold on
      XC = p(1,NACHBAR(2:3,I)); YC = p(2,NACHBAR(2:3,I));
      plot(XC,YC,'r*'), hold on
      XD = [XA; XA + NORMNEU(1,I)]; YD = [YA; YA + NORMNEU(2,I)];
      plot(XD,YD,'r','linewidth',2), hold on
      XE = p(:,NACHBAR(2,I)); YE = p(:,NACHBAR(3,I));
      XF = XE + NACHBAR(5,I)*(YE - XE);
      XG = [XE,XF];
      plot(XG(1,:),XG(2,:),'k','linewidth',2), hold on
      pause(0.2)
   end
case 4, disp(' Randelemente ')
   %axis([0 10 0 10])
   L_RDEL = length(RDEL)
   for I = 1:length(RDEL)
      J = t(1:3,RDEL(I));
      XA = p(1,J); YA = p(2,J);
      fill(XA,YA,'y','erasemode','none'), hold on
   end
   axis off
case 5, disp(' Randknoten ')
   KK = 1;
   for I = 1:size(E,2)
      if KK == 1
         XA = p(1,E(1,I)); YA = p(2,E(1,I));
         plot(XA,YA,'k*'), hold on
         K = find(RDKN(:,I) ~= 0);
         L = RDKN(K,I); XB = p(1,L); YB = p(2,L);
         for J = 1:length(L)
            plot(XB(J),YB(J),'ko'), hold on
            pause(0.2)
         end
      end
      if KK == 0
         XA = p(1,E(1,I)); YA = p(2,E(1,I));
         plot(XA,YA,'r*'), hold on
         K = find(RDKN(:,I) ~= 0);
         L = RDKN(K,I); XB = p(1,L); YB = p(2,L);
         for J = 1:length(L)
            plot(XB(J),YB(J),'ro'), hold on
            pause(0.2)
         end
      end
      if KK == 1, KK = 0; else KK = 1; end
   end
case 6, disp(' Stellen der Randwerte fuer Z ')
   RANDZ = RDZ(1,:);
   for I = 1:length(RANDZ)
      plot(p(1,RANDZ(I)),p(2,RANDZ(I)),'k*'), hold on
      pause(0.2)
   end
case 7, disp(' Stellen der Randwerte fuer W ')
   J = find(RDW(2,:) == 1);
   RANDW1 = RDW(1,J);
   for I = 1:length(RANDW1)
      plot(p(1,RANDW1(I)),p(2,RANDW1(I)),'ro'), hold on
      pause(0.2)
   end
case 8, disp(' Stellen der Randwerte fuer WBOUND')
   J = find(RDW(2,:) == 2);
   RANDW2 = RDW(1,J);
   for I = 1:length(RANDW2)
      plot(p(1,RANDW2(I)),p(2,RANDW2(I)),'go'), hold on
      pause(0.2)
   end
end
clear
