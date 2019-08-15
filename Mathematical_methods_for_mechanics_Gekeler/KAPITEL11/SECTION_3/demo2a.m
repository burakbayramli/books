function demo2a
% Movie fuer Andrew's Squeezer
% Zuerst DEMO2.M Aufrufen!
clc, clf
axis([-0.09 0.03 -0.03 0.09])
axis equal
grid on
load datenfilm FILM;
movie(FILM,3,6)
