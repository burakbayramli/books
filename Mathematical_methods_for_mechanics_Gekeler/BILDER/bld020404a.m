% BILD026A, Stabilitaetsbereich
clf
c = 0.1; d = 0.05;
% -- Raender ----
T = linspace(0,2*pi,60);
X = -1 + cos(T); Y = sin(T);
plot(X,Y,'k','linewidth',2), hold on
fill(X,Y,'y'), hold on
% -- X-Achse ------------
X = [-2.5,1]; Y = [0,0];
arrow(X,Y,c,d,'k',2), hold on
% -- Y- Achse ---------
X = [0,0]; Y = [-2,2];
arrow(X,Y,c,d,'k',2), hold on
circle(-1,0,0.05,'w'), hold on
%axis([-2,2,-2,2])
text(0.24,-0.17,'Re \eta','FontSize',22)
text(-0.71,1.7,'IM \eta','FontSize',22)
text(-1,-0.15,'-1','FontSize',22)
text(-1.1,.5,'S','FontSize',28)

title('(A)','Fontsize',22)
grid off
axis equal tight

