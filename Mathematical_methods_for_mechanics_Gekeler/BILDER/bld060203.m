function bld060203
% Kap. VI, Beispiel 4
clf
aa = 4.5;
c = 0.3;
d = 0.15;
X = [-pi-pi/4,3*pi+pi/4];  %x-Achse
Y = [0,0];
arrow(X,Y,c,d,'k',2)
Y = [aa,aa];               %x-Achse
arrow(X,Y,c,d,'k',2)
X = [0,0];
Y = [-2.5,2.5];            % y-Achse
arrow(X,Y,c,d,'k',2)
X = [0,0];
Y = [aa-1.4,aa+1.4];           % y-Achse
arrow(X,Y,c,d,'k',2)
% Strecke --------------
X = [pi,pi];
Y = [-2, aa+1.4];           % y-Achse
plot(X,Y,'--k','linewidth',2),hold on
% -- Potential ---------------
X = linspace(-pi-pi/4,3*pi+pi/4,40);
Y = aa + - cos(X);
plot(X,Y,'k','linewidth',2),hold on
%-- Heterokline Orbits -----------
C = 2;
X = linspace(-pi-pi/4,pi,50);
Y = sqrt(2*cos(X) + 2);
Z = - Y;
plot(X,Y,'r','linewidth',2),hold on
plot(X,Z,'r','linewidth',2),hold on
X = linspace(pi,3*pi+pi/4,50);
Y = sqrt(2*cos(X) + 2);
Z = - Y;
plot(X,Y,'r','linewidth',2),hold on
plot(X,Z,'r','linewidth',2),hold on
%-- Periodischer Orbit innen-----
X = linspace(-pi/2,pi/2,50);
Y = sqrt(abs(2*cos(X)));
Z = - Y;
plot(X,Y,'g','linewidth',2),hold on
plot(X,Z,'g','linewidth',2),hold on
X = linspace(3*pi/2,5*pi/2,50);
Y = sqrt(abs(2*cos(X)));
Z = - Y;
plot(X,Y,'g','linewidth',2),hold on
plot(X,Z,'g','linewidth',2),hold on
%-- Nichtperiodischer Orbit aussen --------
X = linspace(-5*pi/4,13*pi/4,50);
C = (22/10)^2 - 2;
Y = sqrt(abs(2*cos(X) + C));
Z = - Y;
plot(X,Y,'b','linewidth',2),hold on
plot(X,Z,'b','linewidth',2),hold on
%grid on

c = 0.5;
d = 0.3;
X = [pi,pi+0.2];
e = 0.93;
Y = [e,e];
arrow_2(c,d,X,Y)
hold on
X = [pi,pi-0.2];
e = 0.93;
Y = -[e,e];
arrow_2(c,d,X,Y)
hold on
% -- Punkte -------------------------
circle(0,0,0.15,'w')
circle(2*pi,0,0.15,'w')
circle(pi,0,0.15,'w')
circle(3*pi,0,0.15,'w')
circle(-pi,0,0.15,'w')

text(3*pi+pi/4,0,'x','Fontsize',22)
text(3*pi+pi/4,aa,'x','Fontsize',22)
text(0.2,2.5,'dx/dt','Fontsize',22)
text(0.2,aa+1.4,'U','Fontsize',22)
text(pi,-2.5,'\pi','Fontsize',22)
%title('Skizze zu Beispiel 4, \omega ^2 = 1','Fontsize',22)
axis equal tight
axis off
grid on
