% BILD020303, Schwerpunktkoordinaten
clf
X = [1 0; 9 0; 7 6; 1  0];
P = [6 2];
plot(X(:,1),X(:,2),'LineWidth',2),hold on
X1 = [1 0;6 2];
plot(X1(:,1),X1(:,2),'LineWidth',2),hold on
X2 = [9 0; 6 2];
plot(X2(:,1),X2(:,2),'Linewidth',2),hold on
X3 = [7 6; 6 2];
plot(X3(:,1),X3(:,2),'Linewidth',2),hold on
rr = 0.15;
circle(X(1,1),X(1,2),rr,'w')
circle(X(2,1),X(2,2),rr,'w')
circle(X(3,1),X(3,2),rr,'w')
circle(P(1),P(2),rr,'w')
grid on
axis([0 10 -2 8])
text(0.2,-0.7,'P_1(x_1,y_1)','Fontsize',26)
text(8,-0.7,'P_2(x_2,y_2)','Fontsize',26)
text(7.5,5.7,'P_3(x_3,y_3)','Fontsize',26)
text(6.9,3.1,'T_1','Fontsize',26)
text(4.7,2.6,'T_2','Fontsize',26)
text(5.5,0.7,'T_3','Fontsize',26)
text(6.4,2.2,'P(x,y)','Fontsize',24)
plot(0,-1,'.','color','w')
plot(11,6,'.','color','w')
axis equal tight
axis off
grid off
