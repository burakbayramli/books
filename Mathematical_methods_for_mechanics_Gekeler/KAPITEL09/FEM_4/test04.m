function test01
% Testen von Geometrie und Randbedingungen
% SPEZIELL VON WBOUND.M
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
   REFINE = 3; % Anzahl Gitterverfeinerungen
   SEGNR  = [1,2,3,4];  % Seg.-nrn. Aussenrand  geordnet
   SEGNR1 = [];         % Seg.-nrn. Innenerand
end
Parmeter = [0,0,VS,0];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clc, ecode = 0; bilda = 100; KK = [0,1,2,3,4,5,6,7,8];
%while ~ismember(bilda,KK)
%   bilda = input(' Welches Bild?, (0/1/2/3/4/5/6/7/8) ');
%end;
[p,e,t,RAND] = prepar(FF2,REFINE,[SEGNR,SEGNR1]);
% -- Computation of offset data
[RDKN,RDEL,OFFSET,MAXL] = mesh40(p,e,SEGNR,t);
LAENGE = 1.2*MAXL;
[NACHBAR,NORMALEN,ecode] = mesh43(p,e,SEGNR,OFFSET,LAENGE);
DATA = bsp10_data(p);
Z_EXACT = DATA(1,:)'; W_EXACT = DATA(2,:)'; T_EXACT = DATA(3,:)';
% -- boundary data ------------------------
[RDZ,RCZ,RDW,RDT,RCT] = bsp10h1(p,e,t,T_EXACT,W_EXACT);
WBA  = wbound(p,e,RAND,t,RDW,W_EXACT,Z_EXACT,NACHBAR);
WERTE = [W_EXACT(WBA(1,:)),WBA(2,:)']

