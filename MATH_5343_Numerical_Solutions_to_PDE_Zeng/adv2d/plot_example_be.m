c1 = 2.0; c2 = 1.0;
N1 = 50; N2 = 70;
cfl = [0.99 2.0 5.0];

dt = 'be';

for m = 1 : length(cfl)
  figure('units','normalized','outerposition',[0 0 1 1]);
  [x, y, u] = adv2d_cauchy(N1,N2,cfl(m),c1,c2,dt,'upw');
  [X, Y] = meshgrid(x, y); U = u';
  surf(X,Y,U);
  colormap('gray');
  alpha 0.8;
  xlim([0 1]);
  ylim([0 1]);
  zlim([-.2 1.2]);
  xlabel('x','FontSize',36);
  ylabel('y','FontSize',36);
  zlabel('u','FontSize',36);
  set( gca, 'FontSize', 24 );
end

