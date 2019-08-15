% Scheibe 25, Innenzahnradpumpe
clc
% -- Scheibe A, Zwei Kreissegmente, Uebergaenge stetig diff.bar
R   = 3; PHI = pi/6;
X1  = [R;0];
X2  = R*[cos(PHI);sin(PHI)];
X3  = R*[cos(2*PHI);sin(2*PHI)];
M1A = [norm(X1 + X2)/2;0]; R1A = norm(X1 - X2)/2;
NN1 = 13; TT1  = linspace(-pi/2,pi/2,NN1);
SEGM1A = M1A*ones(1,NN1) + R1A*[cos(TT1);sin(TT1)];
cs  = cos(PHI); ss = sin(PHI); DD = [cs, -ss; ss, cs];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
M1B = [R*sin(7*pi/12)/sin(pi/3);0];
R1B = R*sin(pi/12)/sin(pi/3);
NN2 = 11; TT2   = linspace(4*pi/3,2*pi/3,NN2);
SEGM1B = M1B*ones(1,NN2) + R1B*[cos(TT2);sin(TT2)];
cs = cos(PHI); ss = sin(PHI);
DD = [cs, -ss; ss, cs]; SEGM1B = DD*SEGM1B;
SEGM1B = SEGM1B(:,2:size(SEGM1B,2)-1);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SEGM1 =  [SEGM1A,SEGM1B];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cs = cos(pi/3); ss = sin(pi/3); DD = [cs, -ss; ss, cs];
SEGM2  = DD*SEGM1; SEGM3 = DD*SEGM2; SEGM4  = DD*SEGM3;
SEGM5  = DD*SEGM4; SEGM6 = DD*SEGM5;
X      = [SEGM1,SEGM2,SEGM3,SEGM4,SEGM5,SEGM6];
MA = [1.2;0];                  % neuer Mittelpunkt
% -- Ersten Punkt waehlen ----------------
DN = 7; NX = size(X,2);
X = [X(:,DN:NX),X(:,1:DN-1)];
% -- Scheibe A in Position drehen und verschieben --
cs = cos(pi/6); ss = sin(pi/6); DD = [cs, -ss; ss, cs];
X = DD*X;
MA = [1.2;0];
X = MA*ones(1,NX) + X;
% -- Aussenkreis -------------
RC = M1A(1) + R1A + abs(MA(1)); % Radius des Kreises
SKALE = 1;
RC = RC*SKALE;
TT  = linspace(0,2*pi,100);
Z   = RC*[cos(TT);sin(TT)];
% -- Scheibe B -------------------------
SKALE1 = 1.33; SKALE2 = 0.83; SKALE3 = 0.95;
M2A    = [R*sin(7*pi/12)/sin(pi/3);0]*SKALE1;
R2A    = R*sin(pi/12)/sin(pi/3)*SKALE2;
NN3   = 28; TT2   = linspace(-2*pi*SKALE3/3,2*pi*SKALE3/3,NN3);
SEGMB1 = M2A*ones(1,NN3) - R2A*[cos(TT2);sin(TT2)];
SEGMB1 = fliplr(SEGMB1);

PSI1 = 0.15; PSI2 = 0.62
NN4 = 14; TT3 = linspace(PSI1,PSI2,NN4);
SEGMB2 = RC*[cos(TT3);sin(TT3)];
SEGM1 = [SEGMB1,SEGMB2];
cs     = cos(pi/4); ss = sin(pi/4);
DD     = [cs, -ss; ss, cs];


SEGM2  = DD*SEGM1; SEGM3 = DD*SEGM2;
SEGM4  = DD*SEGM3; SEGM5 = DD*SEGM4; SEGM6 = DD*SEGM5;
SEGM7  = DD*SEGM6; SEGM8 = DD*SEGM7;
Y      = [SEGM1,SEGM2,SEGM3,SEGM4,SEGM5,SEGM6,SEGM7,SEGM8];
NY = size(Y,2); DY = 36;
Y  = [Y(:,DY:NY),Y(:,1:DY-1)];
% -- Grafik ----------------------------
Grafik = 1;
if Grafik == 1
   clf, FAKTOR = 3 % fuer Bildgroesse
   LR  = [-FAKTOR*R; FAKTOR*R];
   UO = [-FAKTOR*R; FAKTOR*R];
   axis([LR(1) LR(2) UO(1) UO(2)])
   grid on, axis equal, axis manual, hold on

   plot(X(1,:),X(2,:),'k'), hold on
   grid on, axis equal, axis manual, hold on

   plot(X(1,1),X(2,1),'.k'), hold on
   %plot(Z(1,:),Z(2,:),'b'), hold on
   %plot(Z(1,1),Z(2,1),'.b'), hold on
   plot(Y(1,:),Y(2,:),'r'), hold on
   plot(Y(1,1),Y(2,1),'.r'), hold on
   circle(MA(1),MA(2),0.08,'w')
   flag = 0;
   if flag == 1
      for I = 1:size(X,2)
         plot(X(1,I),X(2,I),'.k'), hold on, pause
      end
   end
end
M = MA; R = R+R1A;
save scheibe24 M R X
M = [0;0]; R = RC; X = Y;
save scheibe25 M R X

