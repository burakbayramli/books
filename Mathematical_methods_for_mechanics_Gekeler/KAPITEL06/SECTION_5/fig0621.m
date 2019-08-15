function bld060507
clc, clf
% Potential V in three body problem
MU = 0.012277471; MU1 = 1 - MU;
plot(1.5,0.3,'k.'), hold on
plot(-1.5,-3.5,'k.'), hold on
axis tight, axis manual
% -- Achsen --------------
c = 0.1; d = 0.06; NN = 100;
X = [-1.5,1.5]; Y = [0,0];
arrow(X,Y,c,d,'k',2), hold on
c = 0.15; d = 0.03; NN = 100;
% ------------------------------
X = [-MU,-MU]; Y = [-3.5,0.3];
plot(X,Y,'k--','linewidth',2), hold on
X = [0,0]; Y = [-3.5,0.3];
plot(X,Y,'k','linewidth',1), hold on
X = [1-MU,1-MU]; Y = [-3.5,0.3];
plot(X,Y,'k--','linewidth',2), hold on

% -- singular points --------------
aa = 0.83;
X = [aa,aa]; Y = [-3.5,0.3];
plot(X,Y,'k--','linewidth',2), hold on

aa = 1.15;
X = [aa,aa]; Y = [-3.5,0.3];
plot(X,Y,'k--','linewidth',2), hold on






% ----------------------------
XX = linspace(-1.5,-0.28,NN);
rho = abs(XX - MU1); sigma = abs(XX + MU);
V = 0.5*XX.*XX + MU./rho + MU1./sigma;
V = - V;
plot(XX,V,'k','linewidth',2), hold on

XX = linspace(0.1,2,100);
rho = abs(XX - MU1); sigma = abs(XX + MU);
V = 0.5*XX.*XX + MU./rho + MU1./sigma;
V = - V;
plot(XX,V,'k','linewidth',2), hold on

XX = linspace(1.01,1.5,100);
rho = abs(XX - MU1); sigma = abs(XX + MU);
V = 0.5*XX.*XX + MU./rho + MU1./sigma;
V = - V;
plot(XX,V,'k','linewidth',2), hold on

grid on
text(-0.55,-0.75,'Earth','fontsize',28)
text(0.45,-0.75,'Moon','fontsize',28)