%                        Program radial
% Sets up meshgrid in the unit disk. Plots the function z = r.
% Can be modified to plot eigenfunctions in the disk which are
% radially symmetric; namely the Bessel functions.


rr = 0: .05: 1;
r = rr';
theta = 0: 2*pi/100: 2*pi;

X = r*cos(theta);
Y = r*sin(theta);

R = r*ones(size(theta));
TH = ones(size(r))*theta;


surf(X,Y,R);shading flat


% zeros of J0: x = 2.4048, 5.5201, 8.6537
% zeros of J1: x = 3.8317, 7.0156, 10.1735
% zeros of J2: x = 5.1356, 8.4172, 11.6198


