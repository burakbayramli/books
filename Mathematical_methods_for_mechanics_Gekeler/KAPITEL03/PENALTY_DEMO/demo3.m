function demo3
% Example Spellucci, p. 397
clf
BETA = 20;
%BETA = 2;
Parmeter = BETA;
m = 60;
X0 = [0;0];
XR = [-1.5,1.5,1.5,-1.5,-1.5]; YR = [-2.5,-2.5,2.5,2.5,-2.5];
plot(XR,YR,'k','linewidth',2), hold on
plot(-0.505,2.505,'w.'), hold on
axis tight, axis  manual
Z = bsp03(X0,5,Parmeter);
W = [[Z(1,:), fliplr(Z(1,:))]; [Z(2,:), fliplr(Z(3,:))]];
fill(W(1,:),W(2,:),'y'), hold on
X = [linspace(-1.5,1.5,m);linspace(-2.5,2.5,m)];
[U,V] = meshgrid(X(1,:),X(2,:));
Z1 = zeros(m,m); Z2 = zeros(m,m); Z3 = zeros(m,m);
for I = 1:m
   U1 = [X(1,I)*ones(1,m);X(2,:)];
   AUX = feval('bsp03',U1,1,Parmeter); % objective function
   Z1(:,I) = AUX';
   AUX = feval('bsp03',U1,2,Parmeter); % constraint
   Z2(:,I) = AUX(1,:)'; Z3(:,I) = AUX(2,:)';
   AUX = feval('bsp03',U1,3,Parmeter); % Zangwill Penalty function 
   %AUX = feval('bsp03',U1,4,Parmeter); % classic Penalty function 
   Z4(:,I) = AUX';
end
X_OPT = [-1;0]; F_OPT = feval('bsp03',X_OPT,1,Parmeter)
W1    = griddata(X(1,:),X(2,:),Z1,U,V);
W2 = griddata(X(1,:),X(2,:),Z2,U,V); W3 = griddata(X(1,:),X(2,:),Z3,U,V);
W4 = griddata(X(1,:),X(2,:),Z4,U,V);
%[C,h] = contour(U,V,W1,[4.7 4.7],'r','linewidth',2);
%clabel(C,h,'manual');
%hold on
plot(X_OPT(1),X_OPT(2),'.','Markersize',6), hold on
contour(U,V,W1,[3 4 F_OPT 6],'k','linewidth',2), hold on
contour(U,V,W2,[0 0],'k','linewidth',2), hold on
contour(U,V,W3,[0 0],'k','linewidth',2), hold on
contour(U,V,W4,[3 3.5 4 5],'r','linewidth',2), hold on
%axis off
clear
