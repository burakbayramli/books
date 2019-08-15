function bld_turk
% Zeichnet Viereck fuer Turek-element
clc, clf,
c = 0.07; d = 0.03;
% -- Rahmen --------------
plot(1,0,'w.'), hold on
plot(9,9,'w.'), hold on
axis equal tight, grid on
X = [4,7,7.5,2,4];
Y = [1,2,7.5,4,1];
plot(X,Y,'k','linewidth',2), hold on
X1 = [(X(1)+X(2))/2, (X(3)+X(4))/2];
Y1 = [(Y(1)+Y(2))/2, (Y(3)+Y(4))/2];
plot(X1,Y1,'k'), hold on
X2 = [(X(2)+X(3))/2, (X(4)+X(1))/2];
Y2 = [(Y(2)+Y(3))/2, (Y(4)+Y(1))/2];
plot(X2,Y2,'k'), hold on
S1 = sum(X(1:4))/4; S2 = sum(Y(1:4))/4;

c = 0.5; d = 0.15;
X3 =[S1,X2(1)]; Y3 = [S2,Y2(1)];
arrow(X3,Y3,c,d,'k',2)
X4 =[S1,X1(2)]; Y4 = [S2,Y1(2)];
arrow(X4,Y4,c,d,'k',2)

rr = 0.1;
circle(S1,S2,rr,'w')
circle(X(1),Y(1),rr,'w')
circle(X(2),Y(2),rr,'w')
circle(X(3),Y(3),rr,'w')
circle(X(4),Y(4),rr,'w')

circle(X1(1),Y1(1),rr,'w')
circle(X1(2),Y1(2),rr,'w')
circle(X2(1),Y2(1),rr,'w')
circle(X2(2),Y2(2),rr,'w')

text(3.5,0.5,'P_1','fontsize',18)
text(7,1.4,'P_2','fontsize',18)
text(7.2,8,'P_3','fontsize',18)
text(1.5,3.4,'P_4','fontsize',18)
text(6.1,3.7,'\xi','fontsize',18)
text(5.1,4.8,'\eta','fontsize',18)
axis off, grid off
