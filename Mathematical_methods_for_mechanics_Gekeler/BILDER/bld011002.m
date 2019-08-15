% BILD009, Kegel und dualer Kegel im R^2
clf
c = 0.35;
d = 0.15;
subplot(1,2,1)
axis([-2 4 -2 4])
hold on
% -- X-Achse ------------
X = [-2,3];
Y = [0,0];
arrow(X,Y,c,d,'k',2), hold on
% -- Y- Achse ---------
X = [0,0];
Y = [-2,3];
arrow(X,Y,c,d,'k',2), hold on

% -- Kegel --------
X3 = [0,4.32*cos(pi/8),4,4.32*cos(3*pi/8),0];
Y3 = [0,4.32*sin(pi/8),4,4.32*sin(3*pi/8),0];
fill(X3,Y3,'y')

X1 = [0, 3*cos(pi/8)];
Y1 = [0, 3*sin(pi/8)];
arrow_4(X1,Y1,c,d,'k',2)
X2 = [0, 2*cos(3*pi/8)];
Y2 = [0, 2*sin(3*pi/8)];
arrow_4(X2,Y2,c,d,'k',2)
grid on
axis equal
% -- Rahmen -----
RX = [-2,4,4,-2,-2];
RY = [-2,-2,4,4,-2];
plot(RX,RY,'k','linewidth',2)
axis off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
subplot(1,2,2)
c = 0.35;
d = 0.15;
axis([-2 4 -2 4])
hold on

% -- Kegel --------
X3 = [0,4.35*cos(-pi/8),4,4.35*cos(5*pi/8),0];
Y3 = [0,4.35*sin(-pi/8),4,4.35*sin(5*pi/8),0];
fill(X3,Y3,'y')

% -- X-Achse ------------
X = [-2,3];
Y = [0,0];
arrow(X,Y,c,d,'k',2), hold on
% -- Y- Achse ---------
X = [0,0];
Y = [-2,3];
arrow(X,Y,c,d,'k',2), hold on

X3 = [0, 3*cos(5*pi/8)];
Y3 = [0, 3*sin(5*pi/8)];
arrow_4(X3,Y3,c,d,'k',2)
X4 = [0, 2*cos(-pi/8)];
Y4 = [0, 2*sin(-pi/8)];
arrow_4(X4,Y4,c,d,'k',2)
grid on
axis equal
% -- Rahmen -----
RX = [-2,4,4,-2,-2];
RY = [-2,-2,4,4,-2];
plot(RX,RY,'k','linewidth',2)
axis off
%title('Dualer Kegel im R^2','fontsize',24)

