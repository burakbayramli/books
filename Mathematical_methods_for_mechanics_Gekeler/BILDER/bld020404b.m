% BILD026B, Stabilitaetsbereich
clf
c = 0.15; d = 0.07;
% -- Raender ----
T = linspace(0,2*pi,100);
X = 1 + cos(T); Y = sin(T);
plot(X,Y,'k','linewidth',2), hold on
fill(X,Y,'y'), hold on
% -- X-Achse ------------
X = [-0.5,3]; Y = [0,0];
arrow(X,Y,c,d,'k',2), hold on
% -- Y- Achse ---------
X = [0,0]; Y = [-2,2];
arrow(X,Y,c,d,'k',2), hold on
c = 0.05;
circle(1,0,c,'w'), hold on
%axis([-2,2,-2,2])
text(2.3,-0.15,'Re \eta','FontSize',22)
text(0.2,1.7,'IM \eta','FontSize',22)
text(1,-0.2,'1','FontSize',22)
text(0.2,0.25,'Komplement','FontSize',22)
text(0.7,-0.5,'von S','FontSize',22)

title('(B)','Fontsize',22)
grid off
axis equal tight

