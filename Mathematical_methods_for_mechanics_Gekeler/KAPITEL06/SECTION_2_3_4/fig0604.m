function fig0604
% Figure 6.4, Second Kepler's law
% complete ellipse in polar coordinates
% piecewise solution by differential system

clc, clf
disp(' Call first demo1b.m if not done ')
load daten1 TEND Y Parmeter Y1 Y2 Y3
a = Parmeter(5); b = Parmeter(6); e = sqrt(a^2 - b^2);
%%% Ellipse geometrisch %%%%%%%%%%%%%%%%%%%%%%%
PHI = linspace(0,2*pi,50);
XA = e + a*cos(PHI); YA = b*sin(PHI);
%plot(XA,YA,'r','linewidth',1), hold on
%%% Ellipse physikalisch%%%%%%%%%%%%%%%%%%%%%55
[XD,YD]   = pol2cart(Y(:,1),Y(:,2));
plot(XD,YD,'k','linewidth',1), hold on
%plot(XD,YD,'ko','markersize',8), hold on
%%%% 1. Sektor %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[XB,YB]   = pol2cart(Y1(:,1),Y1(:,2));
plot(XB,YB,'b','linewidth',2), hold on
%%%% 2. Sektor %%%%%%%%%%%%%%%%%%%%%%%%%%%%
[XC,YC]   = pol2cart(Y2(:,1),Y2(:,2));
plot(XC,YC,'b','linewidth',2), hold on
%%%% 3. Sektor %%%%%%%%%%%%%%%%%%%%%%%%%%%%
[XD,YD]   = pol2cart(Y3(:,1),Y3(:,2));
plot(XD,YD,'b','linewidth',2), hold on
%% Radius Vectors %%%%%%%%%%%%%%%%%%%%%%%%%
XR1 = [0,XB(1)]; YR1 = [0,YB(1)];
plot(XR1,YR1,'k','linewidth',2), hold on
%% Radius Vectors  1%%%%%%%%%%%%%%%%%%%%%%%%%
XR1 = [0,XB(1)]; YR1 = [0,YB(1)];
plot(XR1,YR1,'k','linewidth',2), hold on
XR2 = [0,XB(end)]; YR2 = [0,YB(end)];
plot(XR2,YR2,'k','linewidth',2), hold on
%% Radius Vectors 2 %%%%%%%%%%%%%%%%%%%%%%%%%
XR3 = [0,XC(1)]; YR3 = [0,YC(1)];
plot(XR3,YR3,'k','linewidth',2), hold on
XR4 = [0,XC(end)]; YR4 = [0,YC(end)];
plot(XR4,YR4,'k','linewidth',2), hold on
%% Radius Vectors 2 %%%%%%%%%%%%%%%%%%%%%%%%%
XR5 = [0,XD(1)]; YR5 = [0,YD(1)];
plot(XR5,YR5,'k','linewidth',2), hold on
XR6 = [0,XD(end)]; YR6 = [0,YD(end)];
plot(XR6,YR6,'k','linewidth',2), hold on
%%% FILL %%%%%%%%%%%%%%%%%%%%%%%%%%%
XF1 = [0;XB;0]; YF1 = [0;YB;0];
fill(XF1,YF1,'y'), hold on
XF2 = [0;XC;0]; YF2 = [0;YC;0];
fill(XF2,YF2,'y'), hold on
XF3 = [0;XD;0]; YF3 = [0;YD;0];
fill(XF3,YF3,'y'), hold on
%%% Rahmen %%%%%%%%%%%%%%%%%%%%
XS = [-0.2,2,2,-0.2,-0.2];
YS = [-0.7,-0.7,0.7,0.7,-0.7];
plot(XS,YS,'k','linewidth',2),hold on

% -- Achsen ---------------------
%c = 0.08; d = 0.03; ZUSATZ = 0.2;
%XACHSE = [-a+e-ZUSATZ,a+e+ ZUSATZ]; YACHSE = [0, 0];
%arrow(XACHSE,YACHSE,c,d,'k',2),hold on
%XACHSE = [0,0]; YACHSE = [-b-ZUSATZ,b+ZUSATZ];
%arrow(XACHSE,YACHSE,c,d,'k',2),hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
[XD,YD]   = pol2cart(Y(:,1),Y(:,2));
plot(XD,YD,'k','linewidth',1), hold on
plot(XD,YD,'ko','markersize',6), hold on

axis equal, grid on
axis off
