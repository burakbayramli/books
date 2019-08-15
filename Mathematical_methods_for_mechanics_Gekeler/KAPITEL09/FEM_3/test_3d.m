function test_3d
clc, clear, clf
n = 10;
theta = (1:n)/n*2*pi;
theta = theta';
X = []; Y = [];
for I = 1:n;


X = [X,theta(I)*cos(theta)];
Y = [Y,theta(I)*sin(theta)];
end
Z = theta*[1:n];
surf(X,Y,Z)

