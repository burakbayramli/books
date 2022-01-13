c = 1.0; nu = 0.01;
N = 20; %100;
M = [13 130]; %[60 600];

[x_ref, u_ref] = adv_diff_cauchy_sol(1000,3,c,nu);

dt = 'bw';

for m = 1 : length(M)
  [x_bw, u_bw] = adv_diff_cauchy(N,M(m),c,nu,dt,'bw','cd');
  [x_fw, u_fw] = adv_diff_cauchy(N,M(m),c,nu,dt,'fw','cd');
  [x_cd, u_cd] = adv_diff_cauchy(N,M(m),c,nu,dt,'cd','cd');

  figure('units','normalized','outerposition',[0 0 1 1]);
  plot(x_bw, u_bw, 'kx--', ...
       x_cd, u_cd, 'k*-.', ...
       x_fw, u_fw, 'ks:', ...
       x_ref, u_ref, 'k-', 'LineWidth', 4, 'MarkerSize', 12);
  xlim([0 1]);
  ylim([-.2 .9]);
  xlabel('x','FontSize',36);
  ylabel('u','FontSize',36);
  leg = legend('backward+central', 'central+central', 'forward+central', 'reference', 'Location', 'NW');
  set( leg, 'FontSize', 24 );
  set( gca, 'FontSize', 24 );
end
