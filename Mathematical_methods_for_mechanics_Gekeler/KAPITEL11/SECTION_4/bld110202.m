function bld110202
clc, clear, clf, hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Daten -----------------
R1 = 1; R2 = 0.8; alfa = pi/4;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
MA = [-R1,0];
MB = [R2,0];
% -- Bildjustierung -----------------------
plot(-1.5,-0.5,'w.'), hold on
plot(1,1.2,'w.'), hold on
axis equal tight, axis manual, grid on
c = 0.1; d = 0.03;
X1 = [-2,2; 0,0];
arrow(X1(1,:),X1(2,:),c,d,'k',2), hold on

% -- Grosser Kreis --------
TT = linspace(0,2*pi,80);
X = MA(1) + cos(TT); Y = MA(2) + sin(TT);
plot(X,Y,'r','linewidth',2), hold on

% -- Kleiner Kreis
X = MB(1) + R2*cos(TT); Y = MB(2) +R2*sin(TT);
plot(X,Y,'r','linewidth',2), hold on

% -- Tangente an Waelzkreis -----------
X4 = [0,0]; Y4 = [-0.5,1];
plot(X4,Y4,'r:','linewidth',2), hold on

% -- Eingriffslinie
a = 0.5; b = 0.75; beta = -pi/2 + alfa;
X5 = [-b,a]; Y5 = [-b*tan(beta),a*tan(beta)];
plot(X5,Y5,'b:','linewidth',2), hold on

% -- Grundkreis A --------------
L = sin(abs(beta));
X6 = [-1,-1 + L*cos(alfa)]; Y6 = [0,L*sin(alfa)];
plot(X6,Y6,'k','linewidth',2), hold on
X7 = -1 + L*cos(TT); Y7 = L*sin(TT);
plot(X7,Y7,'b','linewidth',2), hold on

% -- Grundkreis B --------------
L = R2*sin(abs(beta));
X8 = [R2,R2  - L*cos(alfa)]; Y8 = [0,-L*sin(alfa)];
plot(X8,Y8,'k','linewidth',2), hold on
X9 = R2 + L*cos(TT); Y9 = L*sin(TT);
plot(X9,Y9,'b','linewidth',2), hold on

% -- Aussenkreis A --------
LA = norm(MA - [X8(2),Y8(2)]);
X9 = MA(1) + LA*cos(TT); Y9 = MA(2) + LA*sin(TT);
plot(X9,Y9,'g--','linewidth',2), hold on
% -- Aussenkreis B --------
LB = norm(MB - [X6(2),Y6(2)]);
X9 = R2 + LB*cos(TT); Y9 = LB*sin(TT);
plot(X9,Y9,'g--','linewidth',2), hold on


DD = 0.02;
circle(0,0,DD,'w')
circle(MA(1),MA(2),DD,'w')
circle(MB(1),MB(2),DD,'w')
circle(X6(2),Y6(2),DD,'r')
circle(X8(2),Y8(2),DD,'g')

text(-0.5,1.1,'Aussenkreise','fontsize',18)
text(-1.2,0.9,'Waelzkreis A','fontsize',18)
text(0.2,0.8,'Waelzkreis B','fontsize',18)
text(-1.2,0.6,'Grundkreis A','fontsize',18)
text(0.2,0.5,'Grundkreis B','fontsize',18)
text(-0.13,0.2,'\alpha','fontsize',18)

axis off
