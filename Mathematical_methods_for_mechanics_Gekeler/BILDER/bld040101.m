% BILD058, Transversalitaetsbedingung
clf, clc, clear
% -- X-Achse ----------------------
c = 0.2; d = 0.07;
X1 = [-0.5,3]; Y1 = [0, 0];
arrow(X1,Y1,c,d,'k',2)
% -- Y-Achse -------------------
X1 = [0, 0]; Y1 = [-0.2, 2.5];
arrow(X1,Y1,c,d,'k',2)

% -- Spline-Daten -----------------------
 %Achtung: Spline wird fuer Abszissen 0:LF-1 berechnet!!!
f1= [2, 1.85, 1.7, 1.5, 1.2, 0.8, 0.68];
f2 = [2, 1.53, 1.04, 0.6, 0.4,  0.3, 0.3];
f3 = [-0.3, 0.25, 0.75, 1.1, 1.35,  1.55, 1.7, 1.8];

LF = length(f1);
x1 = linspace(0,LF-1,40);
s1 = spline_n(f1,x1);
s2 = spline_n(f2,x1);
x2 = linspace(0,LF,40);
s3 = spline_n(f3,x2);
x1= x1/2;              % Umskalieren!!
x2 = x2/2;
plot(x1,s1,'k','linewidth',2), hold on
plot(x1,s2,'k','linewidth',2), hold on
plot(x2,s3,'k','linewidth',2), hold on

X3 = 1.17*[1, 1]; Y3 = [-0.25,2];
plot(X3,Y3,'k:','linewidth',2), hold on
X4 = 1.87*[1, 1]; Y4 = [-0.25,2];
plot(X4,Y4,'k:','linewidth',2), hold on

RR = 0.05;
circle(0,2,RR,'w')
circle(1.17,0.88,RR,'w')
circle(1.87,1.29,RR,'w')

% --Rahmen --------------------------
X5 = [-0.7,3.5,3.5,-0.7,-0.7];
Y5 = [-0.7,-0.7,2.7,2.7,-0.7];
plot(X5,Y5,'k','linewidth',2)
text(3.1,0,'t','fontsize',26)
text(-0.25,2.45,'x','fontsize',26)
text(2.8,1.95,'h(t)','fontsize',26)
text(0.2,-0.4,'\phi(x*) = T*','fontsize',26)
text(1.8,-0.4,'t = \phi(x)','fontsize',26)
text(2,0.65,'x*','fontsize',22)
text(2.4,1.15,'x* + \epsilon v','fontsize',26)
text(-0.25,2,'a','fontsize',26)

grid on
axis equal
axis off
