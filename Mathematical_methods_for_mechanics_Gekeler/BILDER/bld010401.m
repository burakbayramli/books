% BILD004 Skizze zu  Abschnitt 1.4, DGLEN
clf
c = 0.2;
d = 0.08;
% -- X-Achse ------------
X = [-0.5,5];
Y = [0,0];
arrow(X,Y,c,d,'k',2)
hold on
% -- Y- Achse ---------
X = [0,0];
Y = [-0.5,1];
arrow(X,Y,c,d,'k',2)
hold on
X = linspace(0,5,60);
Y = X.*exp(-X);
plot(X,Y,'linewidth',2)
hold on
Y = X.*X.*exp(-X);
plot(X,Y,'linewidth',2)
%grid on
text(4.7,-0.25,'x','FontSize',22)
text(0.1,0.8,'y','FontSize',22)
text(0.5,0.6,'y_1','FontSize',22)
text(3,0.7,'y_2','FontSize',22)

%title('y_1 = x exp(-x), y_2 = x^2exp(-x)','Fontsize',18)
axis equal tight
%axis off

