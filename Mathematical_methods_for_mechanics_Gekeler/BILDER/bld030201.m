function bld030201
% BILD041
clf
c = 0.1; d = 0.05;
% -- X-Achse ------------
X = [-0.5,1.5]; Y = [0,0];
arrow(X,Y,c,d,'k',2),hold on
% -- Y- Achse ---------
X = [0,0]; Y = [-0.5,1.5];
arrow(X,Y,c,d,'k',2), hold on
% -- Raender ----
X = [0,0]; Y = [0,1];
plot(X,Y,'k','linewidth',2), hold on
X = [0,1]; Y = [0,0];
plot(X,Y,'k','linewidth',2), hold on
X = linspace(0,1,20); Y = (1 - X).^3;
plot(X,Y,'k','linewidth',2),hold on
% -- Gradienten in x* ------
X = [1,1.3];Y = [0,0];
arrow(X,Y,c,d,'r',2), hold on
X = [1,1]; Y = [0,0.3];
arrow(X,Y,c,d,'r',2), hold on
X = [1,1]; Y = [0,-0.3];
arrow(X,Y,c,d,'r',2), hold on
circle(1,0,0.02,'w');
grid on
%axis([-2,2,-2,2])
text(1.4,0.1,'x','FontSize',22)
text(0.05,1.4,'y','FontSize',22)
text(-0.2,0.5,'g^1','FontSize',22)
text(0.3,-0.1,'g^2','FontSize',22)
text(0.3,0.5,'g^3','FontSize',22)
text(1.1,-0.1,'\nabla f','FontSize',22)
text(1.05,0.25,'\nabla g^3','FontSize',22)
text(1.05,-0.3,'\nabla g^2','FontSize',22)
axis equal tight
grid off
