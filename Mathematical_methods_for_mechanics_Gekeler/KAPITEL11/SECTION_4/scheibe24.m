% Scheibe 25, Innenzahnradpumpe
clc
R     = 3; UB = 2*pi*R;       % Radius/Umfang
M     = [0;0]; FAKTOR = 3;  % Mittelpunkt/Bildgroesse
NN    = 6; PHI = pi/6;
X1    = [R;0];
X2   = R*[cos(PHI);sin(PHI)];
X3   = R*[cos(2*PHI);sin(2*PHI)];
M1    = [norm(X1 + X2)/2;0]; R1 = norm(X1 - X2)/2;
MM    = 13; TT1  = linspace(-pi/2,pi/2,MM);
SEGM1A = M1*ones(1,MM) + R1*[cos(TT1);sin(TT1)];
cs    = cos(PHI); ss = sin(PHI);
DD    = [cs, -ss; ss, cs]; SEGM1A = DD*SEGM1A;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
M2    = [R*sin(7*pi/12)/sin(pi/3);0];
R3    = R*sin(pi/12)/sin(pi/3);
TT2   = linspace(4*pi/3,2*pi/3,MM-2);
SEGM1B = M2*ones(1,MM-2) + R3*[cos(TT2);sin(TT2)];
cs    = cos(pi/4 + PHI/2); ss = sin(pi/4+PHI/2);
DD    = [cs, -ss; ss, cs]; SEGM1B = DD*SEGM1B;
SEGM1B = SEGM1B(:,2:size(SEGM1B,2)-1);
SEGM1  =  [SEGM1A,SEGM1B];
cs     = cos(pi/3); ss = sin(pi/3);
DD     = [cs, -ss; ss, cs];
SEGM2  = DD*SEGM1; SEGM3 = DD*SEGM2;
SEGM4  = DD*SEGM3; SEGM5 = DD*SEGM4; SEGM6 = DD*SEGM5;
X      = [SEGM1,SEGM2,SEGM3,SEGM4,SEGM5,SEGM6];
cs     = cos(pi); ss = sin(pi);
DD     = [cs, -ss; ss, cs];
X = [X, X(:,1)];
M = [-0.6;0]; NX = size(X,2); DN = 5;
X = M*ones(1,NX) + DD*(X - M*ones(1,NX));
X = [X(:,(NX-DN):NX),X(:,1:NX-DN-1)];
%X = X(:,1:NX-1);
% -- Scheibe B -------------------------
SKALE1 = 1.4; SKALE2 = 1.1

R1B = (M1(1) + R1)*SKALE1;
TT = linspace(0,2*pi,100);
Z = R1B*[cos(TT);sin(TT)];

M2    = [-R*sin(7*pi/12)/sin(pi/3);0]*SKALE1;
R3    = R*sin(pi/12)/sin(pi/3);
MM = 14;
TT2   = linspace(2*pi/3,-2*pi/3,2*MM);
SEGM1 = M2*ones(1,2*MM) + R3*[cos(TT2);sin(TT2)];
cs    = cos(pi/4); ss = sin(pi/4);
%DD    = [cs, -ss; ss, cs]; SEGM1 = DD*SEGM1;
%SEGM1 = SEGM1(:,2:size(SEGM1,2)-1);
cs     = cos(pi/4); ss = sin(pi/4);
DD     = [cs, -ss; ss, cs];
SEGM2  = DD*SEGM1; SEGM3 = DD*SEGM2;
SEGM4  = DD*SEGM3; SEGM5 = DD*SEGM4; SEGM6 = DD*SEGM5;
SEGM7 = DD*SEGM6; SEGM8 = DD*SEGM7;
Y      = [SEGM1,SEGM2,SEGM3,SEGM4,SEGM5,SEGM6,SEGM7,SEGM8];
Y = [Y,Y(:,1)];
NY = size(Y,2); DY = 15;
Y = [Y(:,DY:NY),Y(:,1:DY-1)];
%Y = Y(:,1:NY-1);
% -- Grafik ----------------------------
Grafik = 1;
if Grafik == 1
   clf
   LR  = [M(1)-FAKTOR*R; M(1)+FAKTOR*R];
   UO = [M(2)-FAKTOR*R; M(2)+FAKTOR*R];
   axis([LR(1) LR(2) UO(1) UO(2)])
   grid on, axis equal, axis manual, hold on
   plot(X(1,:),X(2,:),'k'), hold on
   plot(X(1,1),X(2,1),'.k'), hold on

   plot(Z(1,:),Z(2,:),'b'), hold on
   plot(Y(1,:),Y(2,:),'r'), hold on
   plot(Y(1,:),Y(2,:),'.r'), hold on

   flag = 0;
   if flag == 1
      for I = 1:size(Y,2)
         plot(Y(1,I),Y(2,I),'.k'), hold on, pause
      end
   end
end
M = [-0.6;0];
save scheibe24 M R X
M = [0;0]; R = R1B; X = Y;
save scheibe25 M R X

