function demo5a
% Movie for top after having called demo5

clc, clf
axis([-2 2 -2 2 -2 2]);
axis equal
%axis manual;
grid on
xlabel('x')
ylabel('y')
zlabel('z')
load datenfilm FILM;
%pause
movie(FILM,5)
