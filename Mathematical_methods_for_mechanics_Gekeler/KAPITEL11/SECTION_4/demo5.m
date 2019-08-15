function dem04
% Film
clc, clf
%axis([-10 10 -10 10])
%axis equal
%axis manual;
%grid on
load datenfilm FILM;
%pause
movie(FILM,5)
