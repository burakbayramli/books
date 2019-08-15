function bld010402
% BILD005 Skizze zu  gedaempfter Schwingung
% Grenzfall "a = 2b"
b = 0.5;
clf
c = 0.2;d = 0.08;
% -- X-Achse ------------
X = [-0.5,2.5];Y = [0,0];
arrow(X,Y,c,d,'k',2),hold on
% -- Y- Achse ---------
X = [0,0]; Y = [-2,0.5];
arrow(X,Y,c,d,'k',2), hold on
X = linspace(1,4,60);
Y = (- X + sqrt(X.*X - 4*b*b))/2;
X = linspace(1,2,60);
Z = (- X - sqrt(X.*X - 4*b*b))/2;
plot(X,Y,'linewidth',2), hold on
plot(X,Z,'linewidth',2), hold on
X = 1; Y = 0;
circle(X,Y,0.04,'w'), hold on
X = 0; Y = - 0.5;
circle(X,Y,0.04,'w'), hold on
X = 1; Y = - 0.5;
circle(X,Y,0.04,'k')
X = [-0.3,1.7];
Y = [0.3,-1.7];
plot(X,Y,'k','linewidth',2)
grid on
text(0.1,0.3,'\lambda','FontSize',26)
text(2.3,0.2,'a','FontSize',26)
text(0.9,0.2,'2b','FontSize',26)
text(-0.4,-0.4,'- b','FontSize',26)
text(1.5,-0.28,'\lambda_1(a)','FontSize',26)
text(1.5,-1.15,'\lambda_2(a)','FontSize',26)
text(0.8,-1.6,'\lambda = -a','Fontsize',26)

axis equal tight
%axis off
grid off
