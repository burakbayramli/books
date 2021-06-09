function demoSurfTypes(n)
% demoSurfTypes Different types of surface plots for z = 2 - x^2 - y^2
%
% Synopsis:  demoSurfTypes(n)
%
% Input:     n = (optional) number of grid lines in x and y directions
%                Default: n = 10
%
% Output:    Surface plot

if nargin<1,  n=10;  end

x = linspace(-5,5,n);
[XG,YG] = meshgrid(x,x);
Z = 2 - (XG.^2 + YG.^2);

subplot(2,2,1)
mesh(x,x,Z);     title('mesh plot');

subplot(2,2,2)
surf(x,x,Z);     title('surf plot');

subplot(2,2,3)
surfc(x,x,Z);    title('surfc plot');

subplot(2,2,4)
surfl(x,x,Z);    title('surfl plot');

colormap('gray');
