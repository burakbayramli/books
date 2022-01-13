c = 1.0; nu = 0.01;
N = 20; %100;
M = [13 130]; %[297 600];

[x_ref, u_ref] = adv_diff_cauchy_sol(1000,3,c,nu);

for m = 1 : length(M)
  [x_mp, u_mp] = adv_diff_cauchy_midpoint(N,M(m),c,nu);

  figure('units','normalized','outerposition',[0 0 1 1]);
  plot(x_mp, u_mp, 'k*-.', ...
       x_ref, u_ref, 'k-', 'LineWidth', 4, 'MarkerSize', 12);
  xlim([0 1]);
  ylim([-.2 .9]);
  xlabel('x','FontSize',36);
  ylabel('u','FontSize',36);
  leg = legend('mid-point rule', 'reference', 'Location', 'NW');
  set( leg, 'FontSize', 24 );
  set( gca, 'FontSize', 24 );
end
