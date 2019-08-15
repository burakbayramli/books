function bild03
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
load daten p e t NORMALEN NACHBARN OFFSET segnr1 segnr2 DIST FAKTOR RDEL RDKN
ecode = 0; bilda = 100;
while ~ismember(bilda,[0,1,2,3,4,5,6])
   bilda = input(' Welches Bild?, (0/1/2/3/4/5/6) ');
end;
%bilda = 0;
bild01(p,e,t,segnr1,segnr2,DIST)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
   NORMALEN(2:3,:) = FAKTOR*NORMALEN(2:3,:);
   for I = 1:size(NORMALEN,2)
      AUX = NORMALEN(1,I);
      XA = p(1,AUX); YA = p(2,AUX);
      plot(XA,YA,'k*'), hold on
      XB = [XA; XA + NORMALEN(2,I)]; YB = [YA; YA + NORMALEN(3,I)];
      plot(XB,YB,'r','linewidth',2), hold on
      pause(0.2)
   end
case 3, disp(' Normalen und Nachbarn ')
   NN = size(NACHBARN,2);
   NORMNEU = zeros(2,NN);
   for I = 1:size(NACHBARN,2)
      NORMNEU(:,I) = NACHBARN(4,I)*NORMALEN(2:3,I);
   end
   for I = 1:NN
      AUX = NORMALEN(1,I);
      XA = p(1,AUX); YA = p(2,AUX);
      plot(XA,YA,'k*'), hold on
      XC = p(1,NACHBARN(2:3,I)); YC = p(2,NACHBARN(2:3,I));
      plot(XC,YC,'r*'), hold on
      XD = [XA; XA + NORMNEU(1,I)]; YD = [YA; YA + NORMNEU(2,I)];
      plot(XD,YD,'r','linewidth',2), hold on
      XE = p(:,NACHBARN(2,I)); YE = p(:,NACHBARN(3,I));
      XF = XE + NACHBARN(5,I)*(YE - XE);
      XG = [XE,XF];
      plot(XG(1,:),XG(2,:),'k','linewidth',2), hold on
      pause(0.1)
   end
case 4, disp(' Randelemente ')
   L_RDEL = length(RDEL)
   for I = 1:length(RDEL)
      J = t(1:3,RDEL(I));
      XA = p(1,J); YA = p(2,J);
      fill(XA,YA,'y','erasemode','none'), hold on
   end
case 5, disp(' Randknoten ')
   EE1 = [];
   if ~isempty(segnr1)
      for I = 1:length(segnr1)
         J = find(e(5,:) == segnr1(I)); EE1 = [EE1,e(1:2,J)];
      end
   end
   EE2 = [];
   if ~isempty(segnr2)
      for I = 1:length(segnr2)
         J = find(e(5,:) == segnr2(I)); EE2 = [EE2,e(1:2,J)];
      end
   end
   E = [EE1,EE2]; KK = 1;
   for I = 1:size(RDKN,2)
      XA = p(1,E(1,I)); YA = p(2,E(1,I));
         plot(XA,YA,'k*'), hold on
         K = find(RDKN(:,I) ~= 0);
         L = RDKN(K,I); XB = p(1,L); YB = p(2,L);
         for J = 1:length(L)
            if KK == 1, plot(XB(J),YB(J),'ko'), hold on, end
            if KK == 0, plot(XB(J),YB(J),'ro'), hold on, end
            pause(0.2)
         end
      if KK == 1, KK = 0; else KK = 1; end
   end
case 6, disp(' Offset-Punkte ')
   X1 = p(1,OFFSET); Y1 = p(2,OFFSET);
   for I = 1:length(OFFSET)
   plot(X1(I),Y1(I),'ro','markersize',6), hold on
   pause(0.2)
   end
end
clear
