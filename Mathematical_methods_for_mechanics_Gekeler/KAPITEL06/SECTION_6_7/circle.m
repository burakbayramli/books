function circle(X,Y,R,farbe)
% CIRCLE.M ---------------
% Zeichnet Kreisscheibe
% X,Y Zentrumkoordinaten
% R Radius
T = linspace(0,2*pi,50);
U = X + R*cos(T);
V = Y + R*sin(T);
plot(U,V), hold on
fill(U,V,farbe,'erasemode','none'), hold on
