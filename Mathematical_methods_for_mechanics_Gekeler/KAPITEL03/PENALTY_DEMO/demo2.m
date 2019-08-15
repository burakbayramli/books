function demo2
% Example Spellucci, p. 394

clc, clf
m = 100;
GAMMA = 50; % Penalty parameter;
Parmeter = GAMMA;
XR = [0,1,1,0,0]; YR = [1,1,2,2,1];
plot(XR,YR,'k','linewidth',2), hold on
plot(-0.005,2.005,'w.'), hold on
axis equal tight, axis  manual
X = [linspace(-1,2,m);linspace(0,3,m)];
[U,V] = meshgrid(X(1,:),X(2,:));
Z1 = zeros(m,m); Z2 = zeros(m,m); Z3 = zeros(m,m);
for I = 1:m
   U1 = [X(1,I)*ones(1,m);X(2,:)];
   AUX = feval('bsp02',U1,1,Parmeter); % objective function
   Z1(:,I) = AUX';
   AUX = feval('bsp02',U1,2,Parmeter); % constraint
   Z2(:,I) = AUX';
   AUX = feval('bsp02',U1,3,Parmeter); % Zangwill function 
 %  AUX = feval('bsp02',U1,4,Parmeter); % classic objective function 
   Z3(:,I) = AUX';
end
X_OPT = [0.5;1.5]; F_OPT = feval('bsp02',X_OPT,1,Parmeter)
W1 = griddata(X(1,:),X(2,:),Z1,U,V);
W2 = griddata(X(1,:),X(2,:),Z2,U,V);
W3 = griddata(X(1,:),X(2,:),Z3,U,V);
contour(U,V,W1,[4,4.25,F_OPT,4.75,5,5.25],'k','linewidth',2), hold on
contour(U,V,W2,[0 0],'b','linewidth',2), hold on
contour(U,V,W3,[4.7 5.5 7],'r','linewidth',2), hold on
circle(X_OPT(1),X_OPT(2),0.015,'w'), hold on
%text(1.5,2.45,'f','fontsize',28)
%text(1.5,1.55,'f','fontsize',28)
%text(1.5,0.2,'h','fontsize',28)
%text(-0.6,2.8,'h','fontsize',28)
%text(0.55,1.7,'P','fontsize',28)

grid on
axis off
