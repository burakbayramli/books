function bld060802
% Bild zu Andrew's Squeezer (Siebenkoerperproplem)
% Bild fuer beta = pi/4
clf
% Geometrische Daten -----------------------
d  = 0.028;    da = 0.0115;   e = 0.02;
ea = 0.01421;  zf = 0.02;    fa = 0.01421;
rr = 0.007;    ra = 0.00092; ss = 0.035;
sa = 0.01874;  sb = 0.01043; sc = 0.018;
sd = 0.02;     zt = 0.04;    ta = 0.02308;
tb = 0.00916;   u = 0.04;    ua = 0.01228;
ub = 0.00449;  c0 = 4530;    L0 = 0.07785;

% -- Werte fuer BETA = pi/4 -------------
X = zeros(7,1);
X = [0.6620, -0.5329, 0.4148, 0.1422, 0.4959, -0.1422, 1.2008];

sibe = sin(X(1)); cobe = cos(X(1));
sith = sin(X(2)); coth = cos(X(2));
siga = sin(X(3)); coga = cos(X(3));
siph = sin(X(4)); coph = cos(X(4));
side = sin(X(5)); code = cos(X(5));
siom = sin(X(6)); coom = cos(X(6));
siep = sin(X(7)); coep = cos(X(7));

sibeth = sin(X(1)+X(2)); cobeth = cos(X(1)+X(2));
siphde = sin(X(4)+X(5)); cophde = cos(X(4)+X(5));
siomep = sin(X(6)+X(7)); coomep = cos(X(6)+X(7));

% Koerper (Angriffspunkte in lokalen Koordinaten
K1  = [-ra, (rr-ra); 0, 0];
K2  = [-(d-da), da; 0, 0];
K3  = [ -sb, sd-sb, -sb; -(ss-sa), sa-sc, sa];
K4  = [ 0, 0; -ea, e-ea];
K5  = [-ta, zt-ta; -tb, -tb];
K6  = [-(zf-fa), fa; 0, 0];
K7  = [ ub, ub; -(u-ua), ua];
flag = 0;
% Koerper in lokalen Koordinaten zeichnen -------
if flag == 1
   clf
   for I = 1:3
      XX = [0,K3(1,I)]; YY = [0,K3(2,I)];
      plot(XX,YY,'k','linewidth',2), hold on
   end
   grid on, axis equal, pause
end
% -- Fixpunkte ---------------------------
FP = [-0.06934, -0.03635,  0.014;
      -0.00227,  0.03273,  0.072];
xa = FP(1,1); xb = FP(1,2);
ya = FP(2,1); yb = FP(2,2);
% Position der Schwerpunkte:
S1 = [ra*cobe; ra*sibe];
S2 = [rr*cobe - da*cobeth;
      rr*sibe - da*sibeth];
S3 = [xb+sa*siga + sb*coga;
      yb-sa*coga + sb*siga];
S4 = [xa+zt*code + (e-ea)*siphde;
      ya+zt*side - (e-ea)*cophde];
S5 = [xa+ta*code - tb*side;
      ya+ta*side + tb*code];
S6 = [xa+u*siep + (zf-fa)*coomep;
      ya-u*coep + (zf-fa)*siomep];
S7 = [xa+ua*siep - ub*coep;
      ya-ua*coep - ub*siep];
% -- PLOTTEN  ----------------------
%clf
rad = 0.001;
rad2 = 0.0015;
% -- Kreis --------------------
TT = linspace(0,2*pi,40);
XX = rr*cos(TT); YY = rr*sin(TT);
plot(XX,YY,'k'), hold on
TT = pi/2;
XX = [rr*cos(TT);rr*sin(TT)];
PHI = pi-0.28;
c = 0.004; d = 0.001;
arrow_8(c,d,XX,PHI)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Position Koerper I -------------
PP  = [ra;0];
K1R = PP*ones(1,size(K1,2)) + K1;
DD  = [cos(X(1)), - sin(X(1)); sin(X(1)), cos(X(1))];
K1D = DD*K1R;
% -- Winkel Zeichnen --------------
XX1 = [0,0.02]; YY1 = [0,0];
plot(XX1,YY1,'k'), hold on
plot(4*K1D(1,:),4*K1D(2,:),'k'), hold on
AA1 = [0,0]; BB1 = [0.015,0]; PHI = 0.68;
[XX1,YY1] = segm(AA1,BB1,PHI,0);
plot(XX1,YY1,'k'), hold on
text(0.01,0.0045,'\beta','fontsize',18)
ZZ = K1D(:,size(K1,2));
XX1 = [ZZ(1),ZZ(1)+0.02]; YY1 = [ZZ(2),ZZ(2)];
plot(XX1,YY1,'k:'), hold on
AA1 = ZZ; BB1 =[ZZ(1)+0.015;ZZ(2)]; PHI = 0.7;
[XX1,YY1] = segm(AA1,BB1,PHI,0);
plot(XX1,YY1,'k'), hold on
text(0.02,0.01,'\Theta','fontsize',18)

plot(K1D(1,:),K1D(2,:),'k','linewidth',2),hold on
circle(K1D(1,1),K1D(2,1),rad,'b')
circle(K1D(1,2),K1D(2,2),rad,'b')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Position Koerper III
NN = size(K3,2);
PP  = FP(:,2) - K3(:,3);
K3R = PP*ones(1,NN) + K3;
DD  = [cos(X(3)), - sin(X(3)); sin(X(3)), cos(X(3))];
K3D = FP(:,2)*ones(1,NN) + DD*(K3R - FP(:,2)*ones(1,NN));
AA = [K3D, K3D(:,1)];
% -- Winkel zeichnen ------------------
ZZ = [FP(1,2),FP(2,2)];
XX1 = [ZZ(1);ZZ(1)]; YY1 = [ZZ(2);ZZ(2)+0.02];
plot(XX1,YY1,'k'), hold on
GAMA = 0.4;
DD  = [cos(GAMA), - sin(GAMA); sin(GAMA), cos(GAMA)];
AUX = DD*[0;0.02];
XX1 = [ZZ(1);ZZ(1)+AUX(1)]; YY1 = [ZZ(2);ZZ(2)+AUX(2)];
plot(XX1,YY1,'k'), hold on
text(-0.04,0.046,'\gamma','fontsize',18)
%------------------------------------
plot(AA(1,:),AA(2,:),'g')
for I = 1:NN
    XX = [S3(1),K3D(1,I)];
    YY = [S3(2),K3D(2,I)];
    plot(XX,YY,'b','linewidth',2), hold on
    %circle(K3D(1,I),K3D(2,I),rad,'g')
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Position Koerper V
NN = size(K5,2);
PP  = FP(:,1) - K5(:,1);
K5R = PP*ones(1,NN) + K5;
DD  = [cos(X(5)), - sin(X(5)); sin(X(5)), cos(X(5))];
K5D = FP(:,1)*ones(1,NN) + DD*(K5R - FP(:,1)*ones(1,NN));
AA = [K5D, K5D(:,1)];
plot(AA(1,:),AA(2,:),'g')
for I = 1:NN
    XX = [S5(1),K5D(1,I)];
    YY = [S5(2),K5D(2,I)];
    plot(XX,YY,'b','linewidth',2), hold on
    %circle(K3D(1,I),K3D(2,I),rad,'g')
end
% -- Winkel Zeichnen --------------
XX1 = [FP(1,1); FP(1,1) + 0.02]; YY1 = [FP(2,1);FP(2,1)];
plot(XX1,YY1,'k'), hold on
text(-0.058,0.0014,'\delta','fontsize',18)
XX1 = [FP(1,1); FP(1,1)]; YY1 = [FP(2,1);FP(2,1)-0.02];
plot(XX1,YY1,'k'), hold on
PHI = 1.2;
AA = [FP(1,1);FP(2,1)]; BB = [FP(1,1);FP(2,1)-0.018];
[XX2,YY2] = segm(AA,BB,PHI,0);
plot(XX2,YY2,'k'), hold on
text(-0.065,-0.015,'\epsilon','fontsize',18)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Position Koerper VII
NN = size(K7,2);
PP  = FP(:,1) - K7(:,2);
K7R = PP*ones(1,NN) + K7;
DD  = [cos(X(7)), - sin(X(7)); sin(X(7)), cos(X(7))];
K7D = FP(:,1)*ones(1,NN) + DD*(K7R - FP(:,1)*ones(1,NN));
AA = [K7D, K7D(:,1)];
plot(AA(1,:),AA(2,:),'g')
for I = 1:NN
    XX = [S7(1),K7D(1,I)];
    YY = [S7(2),K7D(2,I)];
    plot(XX,YY,'b','linewidth',2), hold on
    %circle(K3D(1,I),K3D(2,I),rad,'g')
end
% -- Position Koerper II
K2D = [K3D(:,1),K1D(:,2)];
AA = [K2D,K2D(:,1)];
plot(AA(1,:),AA(2,:),'b','linewidth',2), hold on
circle(K2D(1,1),K2D(2,1),rad,'b')
circle(K2D(1,2),K2D(2,2),rad,'b')

% -- Position Koerper IV
K4D = [K3D(:,1),K5D(:,2)];
AA = [K4D,K4D(:,1)];
plot(AA(1,:),AA(2,:),'b','linewidth',2), hold on
circle(K4D(1,1),K4D(2,1),rad,'b')
circle(K4D(1,2),K4D(2,2),rad,'b')

% -- Position Koerper VI
K6D = [K7D(:,1),K3D(:,1)];
AA = [K6D,K6D(:,1)];
plot(AA(1,:),AA(2,:),'b','linewidth',2), hold on
circle(K6D(1,1),K6D(2,1),rad,'b')
circle(K6D(1,2),K6D(2,2),rad,'b')
% -- Feder zeichnen ------------
XF       = K3D(:,2);
RICHTUNG = FP(:,3) - K3D(:,2);
LAENGE   = norm(RICHTUNG);
Feder    = [XF,FP(:,3)];
Feder1   = [XF,XF+RICHTUNG/4];
Feder2   = [FP(:,3),FP(:,3)-RICHTUNG/4];
%plot(Feder(1,:),Feder(2,:),'r','linewidth',1),hold on
plot(Feder1(1,:),Feder1(2,:),'k','linewidth',2), hold on
plot(Feder2(1,:),Feder2(2,:),'k','linewidth',2), hold on
circle(K3D(1,2),K3D(2,2),rad,'b')
ORTHO = [-RICHTUNG(2);RICHTUNG(1)];
ORTHO = ORTHO/norm(ORTHO);
RLAENGE = 0.1*LAENGE;
DIFF = LAENGE/(2*8);
RICHTUNG = RICHTUNG/LAENGE;
FF1 = Feder1(:,2);
FF2 = Feder1(:,2) + DIFF*RICHTUNG   + RLAENGE*ORTHO;
FF3 = Feder1(:,2) + 2*DIFF*RICHTUNG - RLAENGE*ORTHO;
FF4 = Feder1(:,2) + 3*DIFF*RICHTUNG + RLAENGE*ORTHO;
FF5 = Feder1(:,2) + 4*DIFF*RICHTUNG - RLAENGE*ORTHO;
FF6 = Feder1(:,2) + 5*DIFF*RICHTUNG + RLAENGE*ORTHO;
FF7 = Feder1(:,2) + 6*DIFF*RICHTUNG - RLAENGE*ORTHO;
FF8 = Feder1(:,2) + 7*DIFF*RICHTUNG + RLAENGE*ORTHO;
FF9 = Feder2(:,2);
FF = [FF1,FF2,FF3,FF4,FF5,FF6,FF7,FF8,FF9];
plot(FF(1,:),FF(2,:),'k','linewidth',2), hold on

% -- Schwerpunkte zeichnen -------------
circle(S1(1),S1(2),rad2,'g')
circle(S2(1),S2(2),rad2,'g')
circle(S3(1),S3(2),rad2,'g')
circle(S4(1),S4(2),rad2,'g')
circle(S5(1),S5(2),rad2,'g')
circle(S6(1),S6(2),rad2,'g')
circle(S7(1),S7(2),rad2,'g')
% -- Fixpunkte zeichnen -----------------
circle(0,0,rad,'w');
circle(FP(1,1),FP(2,1),rad2,'r')
circle(FP(1,2),FP(2,2),rad2,'r')
circle(FP(1,3),FP(2,3),rad2,'r')
% -- Fixpunkte bezeichen ------------
text(-0.073,0.004,'A','fontsize',18)
text(-0.0335,0.035,'B','fontsize',18)
text(0.006,0.07,'C','fontsize',18)
% -- Winkel Phi und Omega -----------
text(-0.066,0.03,'pi/2 + \Phi','fontsize',18);
c = 0.005; d = 0.002;
XX = [-0.05;-0.036]; YY = [0.026;0.013];
arrow(XX,YY,c,d,'k',1)

text(-0.01,-0.014,'pi/2 + \Omega','fontsize',18);
c = 0.004; d = 0.0018;
XX = [-0.012;-0.033]; YY = [-.0145;-0.0145];
arrow(XX,YY,c,d,'k',1)
%-- Rahmenpunkte zeichnen -----------
%plot(-0.08,-0.02,'w.','markersize',3), hold on
%plot(0.02,0.08,'w.','markersize',3), hold on
grid on
axis equal tight
grid off
