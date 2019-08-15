function fig0309
% Figure 3.9 with calculation

clc, clf
% -- feasible domain --------
X3 = [2, 2.5,2.5 ,0,0,2]; Y3 = [0,0,2.5,2.5,2,0];
fill(X3,Y3,'y'), hold on
X4 = [-0.2, 2.2]; Y4 = 2 - X4;
plot(X4,Y4,'k','linewidth',2), hold on
% -- x-axis -------------------
c = 0.18; d = 0.07;
X1 = [-2.5, 2.5]; Y1 = [0, 0];
arrow(X1,Y1,c,d,'k',2)
% -- y-axis ------------------
X2 = [0, 0]; Y2 = [-1.5, 2.5];
arrow(X2,Y2,c,d,'k',2)

X    = linspace(-2.5,1,40);
Y    = linspace(-1.5,2,40);
m    = length(X);
Z    = zeros(m,m);
[U,V] = meshgrid(X,Y);
W = 6*U + 2*(U.*U - U.*V + V.*V);
contour(U,V,W,[0 0]), hold on
contour(U,V,W,[-2 -2]), hold on
contour(U,V,W,[-4 -4]), hold on
contour(U,V,W,[6.5 6.5]), hold on

A = [4, -2;-2, 4]; a = [-6;0];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
B = [1, 0; 0, 1; 1, 1]; b = [0;0;-2];
[X,y,f,errorcode,PATH1] = dlqp_demo(A,a,B,b);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
B = [0, 1; 1, 0; 1, 1]; b = [0;0;-2];
[X,y,f,errorcode,PATH2] = dlqp_demo(A,a,B,b);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
B = [1, 1; 1, 0; 0, 1]; b = [-2;0;0];
[X,y,f,errorcode,PATH3] = dlqp_demo(A,a,B,b);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
h = findobj('type','patch');
set(h,'linewidth',2)
plot(PATH1(1,:),PATH1(2,:),'k--','linewidth',2), hold on
plot(PATH2(1,:),PATH2(2,:),'k:','linewidth',2), hold on
plot(PATH3(1,:),PATH3(2,:),'k','linewidth',2), hold on

circle(-2,-1,0.04,'w')
circle(0.5,1.5,0.04,'w')
circle(0,0,0.04,'w')
circle(0,1,0.04,'w')
circle(-1.5,0,0.04,'w')
circle(-1,0,0.04,'w')

text(-2,-1.2,'x_0','fontsize',22)
text(0.1,-0.2,'x_1','fontsize',22)
text(0.1,0.9,'x_2','fontsize',22)
text(0.6,1.6,'x_3','fontsize',22)
text(-1.4,-0.2,'x_4','fontsize',22)
text(-0.8,-0.2,'x_5','fontsize',22)
text(1.5,1.5,'S','fontsize',36)
text(0.8,-0.8,'f(x) = 6.5','fontsize',22)

grid off
axis equal






