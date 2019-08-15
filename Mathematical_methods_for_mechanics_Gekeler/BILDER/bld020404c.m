% BILD026C, Euler explizit: Stabilitaetsbereich
clf
c = 0.15; d = 0.07;
% -- Raender ----
X = [-2.5, 0,0,-2.5,-2.5];
Y = [-2,-2,2,2,-2];
plot(X,Y,'k','linewidth',2), hold on
fill(X,Y,'y'), hold on
% -- X-Achse ------------
X = [-2.5,1]; Y = [0,0];
arrow(X,Y,c,d,'k',2), hold on
% -- Y- Achse ---------
X = [0,0]; Y = [-2,2];
arrow(X,Y,c,d,'k',2), hold on
c = 0.05;
circle(-1,0,c,'w'), hold on
%axis([-2,2,-2,2])
text(0.24,-0.17,'Re \eta','FontSize',22)
text(-0.7,1.7,'IM \eta','FontSize',22)
text(-1,-0.17,'-1','FontSize',22)
text(-1.5,1,'S','FontSize',30)
text(-1.5,-1,'S','FontSize',30)

title('(C)','Fontsize',22)
grid off
axis equal tight

