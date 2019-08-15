% BILD053, Sattelpunkt
clf
[X,Y] = meshgrid(-1:0.1:1);
Z = X.*Y;
mesh(X,Y,Z), hold on
X1 = [-1,1]; Y1 = [0,0]; Z1 = [0,0];
plot3(X1,Y1,Z1,'linewidth',4), hold on
X2 = [0,0]; Y2 = [-1,1]; Z2 = [0,0];
plot3(X2,Y2,Z2,'linewidth',4), hold on

%text(1,1,'K','FontSize',36)
%text(-0.5,-0.3,'A','FontSize',30)
%text(0.25,-0.6,'g(C)','FontSize',30)
%text(-1,0.6,'g(C)','Fontsize',30)
view(30,30)
xlabel('x','fontsize',24)
ylabel('y','fontsize',24)
zlabel('z','fontsize',24)
