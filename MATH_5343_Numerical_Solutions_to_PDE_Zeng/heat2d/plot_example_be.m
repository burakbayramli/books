nu1 = 0.015; nu2 = 0.005;
N1 = 50; N2 = 70;
cfl = [0.99 10.0 100.0];

dt = 'be';

for m = 1 : length(cfl)
  figure('units','normalized','outerposition',[0 0 1 1]);
  [x, y, u] = heat2d_cauchy(N1,N2,cfl(m),nu1,nu2,dt,'cd');
  [X, Y] = meshgrid(x, y); U = u';
  surf(X,Y,U);
  colormap('gray');
  alpha 0.8
  xlim([0 1]);
  ylim([0 1]);
  zlim([-.2 .5]);
  xlabel('x','FontSize',36);
  ylabel('y','FontSize',36);
  zlabel('u','FontSize',36);
  set( gca, 'FontSize', 24 );
end

