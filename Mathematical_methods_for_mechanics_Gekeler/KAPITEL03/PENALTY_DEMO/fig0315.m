function fig0315
% Example Spellucci, p. 457
% same as fig0314 but classical penalty function

clc, clf
m = 60;
BETA1 = 5; BETA2 = BETA1;
Parmeter = [BETA1,BETA2];
% -- Rahmen --------------------------
X0 = [0;0];
XR = [-1.5,1.5,1.5,-1.5,-1.5]; YR = [-1.5,-1.5,1.5,1.5,-1.5];
plot(XR,YR,'k','linewidth',2), hold on
plot(-1.505,1.505,'w.'), hold on
axis equal tight, axis  manual
% -- feasible domain ---------------------------
Z = bsp04(X0,5,Parmeter);
W = [[Z(1,:), fliplr(Z(1,:))]; [Z(2,:), fliplr(Z(3,:))]];
fill(W(1,:),W(2,:),'y'), hold on
X = [linspace(-1.5,1.5,m);linspace(-2.5,2.5,m)];
[U,V] = meshgrid(X(1,:),X(2,:));
Z1 = zeros(m,m); Z2 = zeros(m,m); Z3 = zeros(m,m);
for I = 1:m
   U1 = [X(1,I)*ones(1,m);X(2,:)];
   AUX = feval('bsp04',U1,1,Parmeter); % objective function
   Z1(:,I) = AUX';
   AUX = feval('bsp04',U1,2,Parmeter); % constraint
   Z2(:,I) = AUX(1,:)'; Z3(:,I) = AUX(2,:)';
   %AUX = feval('bsp04',U1,3,Parmeter); % Zangwill 
   AUX = feval('bsp04',U1,4,Parmeter); % classic 

   Z4(:,I) = AUX';
end
W1 = griddata(X(1,:),X(2,:),Z1,U,V);
W2 = griddata(X(1,:),X(2,:),Z2,U,V);
W3 = griddata(X(1,:),X(2,:),Z3,U,V);
W4 = griddata(X(1,:),X(2,:),Z4,U,V);
X_OPT = [0.546;0.702]; F_OPT = feval('bsp04',X_OPT,1,Parmeter)
contour(U,V,W1,[0 0.3 0.6 F_OPT, 0.3],'k','linewidth',2), hold on
contour(U,V,W2,[0 0],'k','linewidth',2), hold on
contour(U,V,W3,[0 0],'k','linewidth',2), hold on
contour(U,V,W4,[0.2, 0.3, 0.6, 1],'r','linewidth',2), hold on
circle(X_OPT(1),X_OPT(2),0.03,'w')
%[C,h] = contour(U,V,W1,[F_OPT F_OPT],'k');
%clabel(C,h,'manual'), hold on
%text(0,0.5,'P','fontsize',28)
text(0,0.5,'Q','fontsize',28)
text(1,1,'f','fontsize',28)
text(-0.9,0.9,'g_2','fontsize',28)
text(-0.9,-0.7,'g_1','fontsize',28)

grid on
axis off
clear
