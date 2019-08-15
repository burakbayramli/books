function fig0312
% Figure 3.12, Example Spellucci, p. 456 for penalty function

clc, clf
BETA = 1;   % Penalty parameter 
X_OPT = 4; % OPtimum
m = 60;
Parmeter = BETA;
X = linspace(-0.5,4.5,m);
m = length(X);
Z1 = zeros(m,m); Z2 = zeros(m,m); Z3 = zeros(m,m);
XR = [-0.5,4.5,4.5,-0.5,-0.5];
YR = [-3,-3,7,7,-3];
plot(XR,YR,'k','linewidth',2), hold on
axis tight manual

plot([4,4],[-2,2],'k--'), hold on
plot([0,0],[-2,2],'k--'), hold on
% objective function ------------------------
Z1 = feval('bsp01',X,1,Parmeter);
plot(X,Z1,'k','linewidth',2), hold on
F_OPT = feval('bsp01',X_OPT,1,Parmeter)
%plot(X_OPT,F_OPT,'b.','markersize',8), hold on
% Side condition 1 --------------------------
Z2 = feval('bsp01',X,2,Parmeter);
plot(X,Z2,'g','linewidth',2), hold on
% Penalty function I-----------------------
Z3 = feval('bsp01',X,3,Parmeter);
plot(X,Z3,'r','linewidth',2), hold on
% Penalty function II----------------------
Z4 = feval('bsp01',X,4,Parmeter);
plot(X,Z4,'b','linewidth',2), hold on
% -----------------------------------------
Z5 = zeros(1,length(X));
plot(X,Z5,'k','linewidth',2)
grid on
text(2,1.5,'f','fontsize',28)
text(2,4.7,'g','fontsize',28)


axis off
