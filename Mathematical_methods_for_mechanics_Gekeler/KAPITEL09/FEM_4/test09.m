function test01
% Testen von Geometrie und Randbedingungen
% fuer Transport Problem
clc, format compact
clc, ecode = 0; 
FF1 = 'bsp09'; FF2 = 'bsp09ha';
REFINE = 1;
VS     = 1;
SEGNR  = [1,2,3,4,5,6];  % Segmentnrn von Aussenrand  geordnet
Parmeter = [0,0,VS,0];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bilda = 100; KK = [0,1,2,3,4,5,6,7,8,9,10];
while ~ismember(bilda,KK)
   bilda = input(' Welches Bild?, (0/1/2/3/4/5/6/7/8/9/10) ');
end;
[p,e,t] = feval(FF1); RAND = e;   % erstes Gitter
for J = 1:REFINE
   [p,e,t] = mesh01_t([],p,e,t);
end   
RAND = e; E = e;
% -- Computation of offset data
[RDKN,RDEL,OFFSET,MAXL] = mesh40(p,e,SEGNR,t);
LAENGE = 1.2*MAXL; % Length of normals in bound for SCHNITT.M
LAENGE = MAXL; % Length of normals in bound for SCHNITT.M

[NACHBAR,NORMALEN,ecode] = mesh43(p,e,SEGNR,OFFSET,LAENGE);
% -- boundary data ------------------------
[RDZ,RDW] = feval(FF2,e,Parmeter);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
save datena p e E t Parmeter
save datenb RDZ RDW NACHBAR NORMALEN
% -- Geometrie -----------------------------
clf, hold on
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
t1 = t(1:3,:);
trimesh(t1',X,Y,Z1,'edgecolor','b'), hold on
% -- Bildausschnitt festlegen --------------
%for I = 1:size(e,2)
%   A = [p(1,e(1,I));p(1,e(2,I))];
%   B = [p(2,e(1,I));p(2,e(2,I))];
%   plot(A,B,'r','linewidth',1), hold on
%end
% -- Randsegmente ---------------
for I = 1:size(E,2)
   A = [p(1,E(1,I));p(1,E(2,I))];
   B = [p(2,E(1,I));p(2,E(2,I))];
   plot(A,B,'k','linewidth',2), hold on
end
axis equal, axis manual, grid on

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Testen der einzelnen Hilfsprogramme ----
switch bilda
case 1
   KK = 1;
   disp(' Offset-Punkte ')
   for I = 1:length(OFFSET)
      XA = p(1,OFFSET(I)); YA = p(2,OFFSET(I));
      if KK == 1
         plot(XA,YA,'k*'), hold on
      end
      if KK == 0
         plot(XA,YA,'ko'), hold on
      end
      if KK == 1, KK = 0; else KK = 1; end
      pause(0.2)
      %pause
   end
case 2
   disp(' Normalen ')
   NORMALEN = LAENGE*NORMALEN;
   for I = 1:size(NORMALEN,2)
      XA = p(1,E(1,I)); YA = p(2,E(1,I));
      plot(XA,YA,'k*'), hold on
      XB = [XA; XA + NORMALEN(2,I)]; YB = [YA; YA + NORMALEN(3,I)];
      plot(XB,YB,'r','linewidth',1), hold on
      pause(0.2)
   end
case 3
   disp(' Normalen und Nachbarn ')
   NN = size(NACHBAR,2);
   NORMNEU = zeros(2,NN);
   for I = 1:size(NACHBAR,2)
      A = p(1:2,E(1,I)); B = A + LAENGE*NORMALEN(2:3,I);
      NORMNEU(:,I) = LAENGE*NACHBAR(4,I)*NORMALEN(2:3,I);
   end
   for I = 1:NN
     % clf, hold on
     % trimesh(t1',X,Y,Z1,'edgecolor','b'), hold on
     % axis([0 0.05 0 0.05])
     % axis equal, axis manual
      XA = p(1,E(1,I)); YA = p(2,E(1,I));
      plot(XA,YA,'k*'), hold on
      XC = p(1,NACHBAR(2:3,I)); YC = p(2,NACHBAR(2:3,I));
      plot(XC,YC,'r*'), hold on
      % -------------------------------------
      XD = [XA; XA + NORMNEU(1,I)]; YD = [YA; YA + NORMNEU(2,I)];
    %  plot(XD,YD,'r','linewidth',2), hold on
      % ------------------------------------
      XE = p(:,NACHBAR(2,I)); YE = p(:,NACHBAR(3,I));
      XF = XE + NACHBAR(5,I)*(YE - XE);
      XG = [XE,XF];
      plot(XG(1,:),XG(2,:),'k','linewidth',2), hold on
      pause(0.2)
   end
case 4
   disp(' Randelemente ')
   L_RDEL = length(RDEL)
   for I = 1:length(RDEL)
      J = t(1:3,RDEL(I));
      XA = p(1,J); YA = p(2,J);
      %plot(XA,YA,'k*'), hold on
      fill(XA,YA,'g','erasemode','none'), hold on
  %   pause
   end
case 5
   disp(' Randknoten ')
   KK = 1;
   for I = 1:size(E,2)
      if KK == 1
         XA = p(1,E(1,I)); YA = p(2,E(1,I));
         plot(XA,YA,'k*'), hold on
         K = find(RDKN(:,I) ~= 0);
         L = RDKN(K,I); XB = p(1,L); YB = p(2,L);
         for J = 1:length(L)
            plot(XB(J),YB(J),'ko'), hold on
            pause(0.3)
         end
      end
      if KK == 0
         XA = p(1,E(1,I)); YA = p(2,E(1,I));
         plot(XA,YA,'r*'), hold on
         K = find(RDKN(:,I) ~= 0);
         L = RDKN(K,I); XB = p(1,L); YB = p(2,L);
         for J = 1:length(L)
            plot(XB(J),YB(J),'ro'), hold on
            pause(0.3)
         end
      end
      if KK == 1, KK = 0; else KK = 1; end
      %pause
   end
case 6
   disp(' Randwerte fuer Z ')
   RANDZ = RDZ(1,:);
   for I = 1:length(RANDZ)
      I;
      plot(p(1,RANDZ(I)),p(2,RANDZ(I)),'k*'), hold on
      pause
   end
case 7
   disp(' Randwerte fuer W ')
   J = find(RDW(2,:) == 1);
   RANDW1 = RDW(1,J);
   for I = 1:length(RANDW1)
      I;
      plot(p(1,RANDW1(I)),p(2,RANDW1(I)),'ro'), hold on
      pause
   end
case 8
   disp(' Randwerte fuer WBOUND')
   J = find(RDW(2,:) == 2);
   RANDW2 = RDW(1,J);
   for I = 1:length(RANDW2)
      I;
      plot(p(1,RANDW2(I)),p(2,RANDW2(I)),'go'), hold on
      pause
   end
case 9
   disp(' Zusaetzliche Randwerte fuer W ')
   if ~isempty(RDWSONDER)
      RANDW1 = RDWSONDER(1,:);
      for I = 1:length(RANDW1)
         I;
         plot(p(1,RANDW1(I)),p(2,RANDW1(I)),'bo'), hold on
         pause
      end
   end
case 10
   disp(' Normalen und Nachbarn fuer WBOUND')
   AUX = zeros(5,size(e,2));
   AUX(:,E(1,:)) = NACHBAR;
   NACHBARWB = AUX(:,EW(1,:));
   AUX = zeros(3,size(e,2));
   AUX(:,E(1,:)) = NORMALEN;
   NORMALENWB = AUX(:,EW(1,:));
   LAENGEN = NACHBARWB(4,:)
   NN = size(NACHBARWB,2);
   NORMNEU = zeros(2,NN);
   for I = 1:size(NACHBARWB,2)
      NORMNEU(:,I) = LAENGE*NACHBARWB(4,I)*NORMALENWB(2:3,I);
   end
   for I = 1:NN
     % clf, hold on
     % trimesh(t1',X,Y,Z1,'edgecolor','b'), hold on
     % axis([2 8 2 8])
     % axis equal, axis manual
      XA = p(1,E(1,I)); YA = p(2,E(1,I));
      plot(XA,YA,'k*'), hold on
      XC = p(1,NACHBARWB(2:3,I)); YC = p(2,NACHBARWB(2:3,I));
      plot(XC,YC,'r*'), hold on
      % -------------------------------------
      XD = [XA; XA + NORMNEU(1,I)]; YD = [YA; YA + NORMNEU(2,I)];
      plot(XD,YD,'k','linewidth',2), hold on
      % ------------------------------------
      XE = p(:,NACHBARWB(2,I)); YE = p(:,NACHBARWB(3,I));
      XF = XE + NACHBARWB(5,I)*(YE - XE);
      XG = [XE,XF];
      plot(XG(1,:),XG(2,:),'k','linewidth',2), hold on
      pause
   end
end
clear
