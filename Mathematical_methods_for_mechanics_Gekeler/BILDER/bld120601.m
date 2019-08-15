function bld120601
% Orthozykloiden
clc, clf
flag = 100;
while ~ismember(flag,[1,2,3])
   flag = input('Welches Bild? (1/2/3)');
end
%flag = 6
switch flag
case 1; r = 2; c = 1;
case 2; r = 2; c = 2;
case 3; r = 2; c = 3;
end
psi = 0;     %Startwert auf Rollkreis
psi = pi;     %Startwert auf Rollkreis
phi = pi/4;   % spezieller Winkel
% -- x-Achse ------------------
X = [-pi*r, 3*pi*r]; Y = [0, 0];
arrow(X,Y,0.6,0.2,'k',1);
% -- y-Achse ----------------------
X = [0, 0]; Y = [-0.2, 2.55*r];
arrow(X,Y,0.5,0.2,'k',1);
% ----------------------------
PHI = linspace(-3*pi/4,11*pi/4,100);
PSI = linspace(0,2*pi,100);
% -- Zykloide ------------
X = r*PHI - c*sin(PHI); Y = r - c*cos(PHI);
plot(X,Y,'k','LineWidth',2), hold on
% -- erster Rollkreis ----------------
phi = 5*pi/8;
X = r*phi + r*cos(PSI); Y = r + r*sin(PSI);
plot(X,Y,'k'), hold on
X1 = r*phi; Y1 = r;
X2 = r*phi - c*sin(phi); Y2 = r - c*cos(phi);
X3 = r*phi; Y3 = 0;
X4 = [X1, X2]; Y4 = [Y1, Y2];
X5 = [X1, X3]; Y5 = [Y1, Y3];
plot(X4,Y4,'k'), hold on
plot(X5,Y5,'k'), hold on
RR = 0.15;
circle(X1,Y1,RR,'w')
circle(X2,Y2,RR,'w')
circle(X3,Y3,RR,'w')
% zweiter Rollkreis -----------------
phi = 2*pi - 3*pi/8;
X = r*phi + r*cos(PSI); Y = r + r*sin(PSI);
plot(X,Y,'k'), hold on
X1 = r*phi; Y1 = r;
X2 = r*phi - c*sin(phi); Y2 = r - c*cos(phi);
X3 = r*phi; Y3 = 0;
X4 = [X1, X2]; Y4 = [Y1, Y2];
X5 = [X1, X3]; Y5 = [Y1, Y3];
plot(X4,Y4,'k'), hold on
plot(X5,Y5,'k'), hold on
circle(X1,Y1,RR,'w')
circle(X2,Y2,RR,'w')
circle(X3,Y3,RR,'w')
gr = 14;
% Beschriftung von Bild 1, r = 2, c = 1
text(18,0.55,'x','Fontsize',gr)
text(-0.7,4.5,'y','Fontsize',gr)
text(9,2,'M','Fontsize',gr)
text(4.5,2,'M','Fontsize',gr)
text(3.2,1.5,'\phi','Fontsize',gr)
flag = 1;
if flag == 1 % Zu Bild 1
   text(2.7,3.1,'P','Fontsize',gr)
   text(11,2.5,'P','Fontsize',gr)
   text(-6,0.6,'r = 2, c = 1','fontsize',gr)
end
if flag == 2 % Zu Bild 2
   text(1.3,3.1,'P','Fontsize',gr)
   text(12.5,1.8,'P','Fontsize',gr)
   text(-6,0.6,'r = 2, c = 2','fontsize',gr)
end
if flag == 3 % Zu Bild 3
   text(1,3.8,'P','Fontsize',gr)
   text(13.5,1,'P','Fontsize',gr)
   text(-6,0.6,'r = 2, c = 3','fontsize',gr)
end
axis equal tight, grid on
axis off
