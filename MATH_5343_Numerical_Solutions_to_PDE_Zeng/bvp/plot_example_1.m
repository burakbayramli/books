c = 1.0; nu = 1.0;

N = [4 8];

x_ref = linspace(0,1,1001);
u_ref = (exp(c*x_ref/nu)-1.0)/(exp(c/nu)-1.0);

for n = 1 : length(N)
  [x_bw, u_bw] = adv_diff_bvp(N(n),c,nu,'bw','cd');
  [x_fw, u_fw] = adv_diff_bvp(N(n),c,nu,'fw','cd');
  [x_cd, u_cd] = adv_diff_bvp(N(n),c,nu,'cd','cd');

  figure('units','normalized','outerposition',[0 0 1 1]);
  plot(x_bw, u_bw, 'kx--', ...
       x_cd, u_cd, 'k*-.', ...
       x_fw, u_fw, 'ks:', ...
       x_ref, u_ref, 'k-', 'LineWidth', 4, 'MarkerSize', 12);
  xlim([0 1]);
  ylim([0 1]);
  xlabel('x','FontSize',36);
  ylabel('u','FontSize',36);
  leg = legend('backward+central', 'central+central', 'forward+central', 'reference', 'Location', 'NW');
  set( leg, 'FontSize', 24 );
  set( gca, 'FontSize', 24 );
end
