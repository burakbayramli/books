function fig0313
% Figure 3.13, Example Spellucci, p. 456 for penalty function

clc, clf
BETA = 1;   % Penalty parameter 
X_OPT = 4; % OPtimum
m = 100;
Parmeter = BETA;
X = linspace(-0.5,4.5,m);
m = length(X);
Z1 = zeros(m,m); Z2 = zeros(m,m); Z3 = zeros(m,m);
XR = [3.5,4.5,4.5,3.5,3.5];
YR = [-2,-2,1,1,-2];
plot(XR,YR,'k','linewidth',2), hold on
text(3.65,-1.5,'f,P,Q','fontsize',28)
text(4.3,-1.8,'f','fontsize',28)
text(4.3,-0.5,'P','fontsize',28)
text(4.25,0.5,'Q','fontsize',28)
axis tight manual

plot([4,4],[-2,2],'k--','linewidth',2), hold on
plot([0,0],[-2,2],'k--'), hold on
% Zielfunktion ------------------------
Z1 = feval('bsp01',X,1,Parmeter);
plot(X,Z1,'k','linewidth',2), hold on
F_OPT = feval('bsp01',X_OPT,1,Parmeter)
plot(4,-1.5,'b.','markersize',20), hold on
% Nebenbedingung1 --------------------------
Z2 = feval('bsp01',X,2,Parmeter);
plot(X,Z2,'g','linewidth',2), hold on
% Penalty-Funktion I-----------------------
Z3 = feval('bsp01',X,3,Parmeter);
plot(X,Z3,'r','linewidth',2), hold on
% Penalty-Funktion II----------------------
Z4 = feval('bsp01',X,4,Parmeter);
plot(X,Z4,'b','linewidth',2), hold on
% -----------------------------------------
Z5 = zeros(1,length(X));
plot(X,Z5,'k','linewidth',2)
grid off
text(2,1.5,'f','fontsize',28)
text(2,4.5,'h','fontsize',28)


%axis off
