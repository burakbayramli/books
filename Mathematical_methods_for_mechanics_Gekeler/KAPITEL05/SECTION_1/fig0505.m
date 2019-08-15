function fig0505
% Figure 5.5, Example 5.3

clc, clf
set(gcf,'renderer','zbuffer')

%Z = 0.5*Y.*(X - Y.^2);
% -- MU-Achse ---------------------
X = [-1,1.2]; Y = [0,0]; Z = [0,0];
plot3(X,Y,Z,'k'), hold on
X = [-1,1]; Y = [0,0]; Z = [0,0];
plot3(X,Y,Z,'k','linewidth',2), hold on

Z = [1,0,0]; A = [1.2,0,0]; r = 0.016; s = 0.07;
PHI = - 0.2; PSI = pi;
kegel4(Z,r,s,A,PHI,PSI), hold on
HH = 0.2;
% -- Parabelzweige --------------------------
Y2 = linspace(1,0,40); X2 = Y2.*Y2; Z2 = HH*Y2.^3;
Y1 = linspace(0,1,40); X1 = Y1.*Y1; Z1 = 0*X1;
fill3([X1,X2],[Y1,Y2],[Z1,Z2],'y','edgecolor','y'), hold on
plot3(X2,Y2,Z2,'k','linewidth',2,'erasemode','none'), hold on

bb = 0.95;
Y1 = linspace(0,bb,20); X1 = Y1.*Y1; Z1 = 0*X1;
plot3(X1,Y1,Z1,'k--','linewidth',2,'erasemode','none'), hold on
Y1 = linspace(bb,1,20); X1 = Y1.*Y1; Z1 = 0*X1;
plot3(X1,Y1,Z1,'k','linewidth',2), hold on

Y1 = linspace(-1,0,20); X1 = Y1.*Y1; Z1 = 0*X1;
Y2 = linspace(0,-1,20); X2 = Y2.*Y2; Z2 = HH*Y2.^3;
fill3([X1,X2],[Y1,Y2],[Z1,Z2],'y','edgecolor','y'), hold on
plot3(X1,Y1,Z1,'k','linewidth',2,'erasemode','none'), hold on
cc = -0.85;

Y2 = linspace(0,3*cc/5,40); X2 = Y2.*Y2; Z2 = HH*Y2.^3;
plot3(X2,Y2,Z2,'k--','linewidth',2,'erasemode','none'), hold on
Y2 = linspace(3*cc/5+0.2*cc/5,3*cc/5+0.6*cc/5,40); X2 = Y2.*Y2; Z2 = HH*Y2.^3;
plot3(X2,Y2,Z2,'k--','linewidth',2,'erasemode','none'), hold on
Y2 = linspace(3*cc/5+0.8*cc/5,3*cc/5+1.1*cc/5,40); X2 = Y2.*Y2; Z2 = HH*Y2.^3;
plot3(X2,Y2,Z2,'k--','linewidth',2,'erasemode','none'), hold on
Y2 = linspace(3*cc/5+1.2*cc/5,3*cc/5+1.5*cc/5,40); X2 = Y2.*Y2; Z2 = HH*Y2.^3;
plot3(X2,Y2,Z2,'k--','linewidth',2,'erasemode','none'), hold on
Y2 = linspace(3*cc/5+1.65*cc/5,3*cc/5+2*cc/5,40); X2 = Y2.*Y2; Z2 = HH*Y2.^3;
plot3(X2,Y2,Z2,'k--','linewidth',2,'erasemode','none'), hold on



Y2 = linspace(cc,-1,40); X2 = Y2.*Y2; Z2 = HH*Y2.^3;
plot3(X2,Y2,Z2,'k','linewidth',2,'erasemode','none'), hold on

% -- Konturen der Flaeche --------------
Y1 = linspace(-1.2,1.2,20); X1 = ones(1,20); Z1 = HH*Y1.^3;
plot3(X1,Y1,Z1,'b','linewidth',2), hold on

Y1 = linspace(0,1.2,20); X1 = -ones(1,20); Z1 = HH*Y1.^3;
plot3(X1,Y1,Z1,'b','linewidth',2), hold on
Y1 = linspace(-1.2,-0.78,20); X1 = -ones(1,20); Z1 = HH*Y1.^3;
plot3(X1,Y1,Z1,'b','linewidth',2), hold on
Y1 = linspace(-0.78,0,20); X1 = -ones(1,20); Z1 = HH*Y1.^3;
plot3(X1,Y1,Z1,'b--','linewidth',2), hold on

Y1 = linspace(0,1.2,20); X1 = zeros(1,20); Z1 = HH*Y1.^3;
plot3(X1,Y1,Z1,'b','linewidth',2), hold on
Y1 = linspace(-1.2,-0.78,20); X1 = zeros(1,20); Z1 = HH*Y1.^3;
plot3(X1,Y1,Z1,'b','linewidth',2), hold on
Y1 = linspace(-0.78,0,20); X1 = zeros(1,20); Z1 = HH*Y1.^3;
plot3(X1,Y1,Z1,'b--','linewidth',2), hold on

X1 = [-1,1]; Y1 = [-1.2,-1.2]; Z1 = - HH*1.2^3*[1,1];
plot3(X1,Y1,Z1,'b','linewidth',2), hold on
X1 = [-1,1]; Y1 = [1.2,1.2]; Z1 = HH*1.2^3*[1,1];
plot3(X1,Y1,Z1,'b','linewidth',2), hold on
% -- Ebene ----------------
X1 = [1,1]; Y1 = [-1.2,1]; Z1 = [0,0];
plot3(X1,Y1,Z1,'k','linewidth',2), hold on
X1 = [1,1]; Y1 = [-1,1.2]; Z1 = [0,0];
plot3(X1,Y1,Z1,'k'), hold on
Z = [0,1,0]; A = [1,1.2,0]; r = 0.016; s = 0.07;
PHI = - 0.6 ; PSI = pi;
kegel4(Z,r,s,A,PHI,PSI), hold on
X1 = [-1,1]; Y1 = [-1.2,-1.2]; Z1 = [0,0];
plot3(X1,Y1,Z1,'k','linewidth',2), hold on
X1 = [-1,0.7]; Y1 = [1,1]; Z1 = [0,0];
plot3(X1,Y1,Z1,'k--','linewidth',2), hold on

X1 = [-1,-1]; Y1 = [-1.2,0]; Z1 = [0,0];
plot3(X1,Y1,Z1,'k','linewidth',2), hold on
X1 = [-1,-1]; Y1 = [0,1]; Z1 = [0,0];
plot3(X1,Y1,Z1,'k--','linewidth',2), hold on
% -- Einzelne Punkte -------------
plot3(-0.6,-1,0,'k.','markersize',12), hold on
plot3(-0.6,-1,-HH,'k.','markersize',12), hold on
XX = [-0.6,-0.6]; YY = [-1,-1]; ZZ = [0,-HH];
plot3(XX,YY,ZZ), hold on 


plot3(-0.6,0.85,0,'k.','markersize',12), hold on
plot3(-0.6,0.85,HH*0.9^3,'k.','markersize',12), hold on
XX = [-0.6,-0.6]; YY = [0.85,0.85]; ZZ = [0,HH*0.85^3];
plot3(XX,YY,ZZ,'k'), hold on 

text(-0.55,-1,0.1,'(\mu,u)','fontsize',22)
text(-0.55,-1,-0.23,'(\mu,u + w)','fontsize',22)

% -- Eckpunkte ------------------
aa = 1.2;
plot3(-aa,-aa,-0.5,'k.'), hold on
plot3(aa,aa,0.5,'k.'), hold on

%xlabel('\mu','fontsize',18)
%ylabel('x','fontsize',18)
text(1.25,0,0,'\mu','fontsize',22)
text(1,1.25,0,'u','fontsize',22)
text(-0.9,0.9,0.25,'(I-Q)F(\mu,u+w) = 0','fontsize',22)
grid on
axis equal
axis off
view(20,20);
