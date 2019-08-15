% BILD003 Schmiegebene
clf
clc
c = 0.1;
d = 0.02;
S = 0.5;
T = linspace(-pi,2.8);
X = [cos(T);sin(T);S*T];
T0 = 0;
DX = [-sin(T0);cos(T0);S];
DDX = [-cos(T0);-sin(T0);0];
Z = X;;
plot3(Z(1,:),Z(2,:),Z(3,:),':','linewidth',2)
hold on
A = [cos(T0);sin(T0);S*T0];
T1 = DX/norm(DX);
T1A = T1  + [0;0;0.01];
X1 = [A,A+T1A]';
myquive(X1(:,1),X1(:,2),X1(:,3),c,d,'k',2,1), hold on
N1 = DDX/norm(DDX);
N1A = N1 + [0;0;0.01];
X1 = [A,A+N1A]';
myquive(X1(:,1),X1(:,2),X1(:,3),c,d,'k',2,1), hold on
B1 = vecprod(T1,N1);
B1 = B1/norm(B1);
X1 = [A,A+B1]';
myquive(X1(:,1),X1(:,2),X1(:,3),c,d,'k',2,1), hold on


X1 = [A+T1+N1,A-T1+N1,A-T1-N1,A+T1-N1,A+T1+N1];
plot3(X1(1,:),X1(2,:),X1(3,:)), hold on
fill3(X1(1,:),X1(2,:),X1(3,:),'w'), hold on
drawnow
%hidden off
%%%%%%%%%%%%%%%%%%%%%%%%%%
set(gca,'NextPlot','add');
K = newplot;
K = plot3(Z(1,:),Z(2,:),Z(3,:),'linewidth',2,'color','g');

grid on
axis equal
text(1.4,0,0.4,'T','fontsize',18);
text(0.3,-0.3,-0.1,'N','fontsize',18);
text(0.5,0,0.4,'B','fontsize',18);

xlabel('x','fontsize',18);
ylabel('y','fontsize',18);
zlabel('z','fontsize',18)
view([30,20])
