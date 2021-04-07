% Specify x range and number of points
x0 = -3;
x1 =  3;
Nx = 301;

% Specify y range and number of points
y0 = -3;
y1 =  3;
Ny = 301;

% Construct mesh
xv    = linspace(x0,x1,Nx);
yv    = linspace(y0,y1,Ny);
[x,y] = meshgrid(xv,yv);

% Calculate z
z = x + i*y;

% 2nd order Runge-Kutta growth factor
g = 1 + z + 0.5*z.^2;

% Calculate magnitude of g
gmag = abs(g);

% Plot contours of gmag
contour(x,y,gmag,[1 1],'k-');
axis([x0,x1,y0,y1]);
axis('square');
xlabel('Real \lambda\Delta t');
ylabel('Imag \lambda\Delta t');
grid on;
