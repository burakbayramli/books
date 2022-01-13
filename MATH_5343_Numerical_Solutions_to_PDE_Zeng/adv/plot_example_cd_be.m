c = 1.0;
N = 80;
cfl = {[0.99 2 5], [0.99 2 5]};
cfl_spec = {'kx--','k*-.','ks:'};

f_ic{1} = @(x)16*(x.*(1-x)).^2;
f_ic{2} = @(x)(x<=0.75).*(x>=0.25);

x_ref = linspace(0,1,1001);

dx = 'cd'; dt = 'be';

for nic = 1 : length(f_ic)
  figure('units','normalized','outerposition',[0 0 1 1]);
  hold on;
  u_ref = f_ic{nic}(x_ref);

  for m = 1 : length(cfl{nic})
    [x_sol, u_sol] = adv_cauchy(N,cfl{nic}(m),c,dt,dx,f_ic{nic});
    plot(x_sol, u_sol, cfl_spec{m}, 'LineWidth', 4, 'MarkerSize', 12);
    legs{m} = sprintf('cfl=%f',cfl{nic}(m));
  end
  plot(x_ref, u_ref, 'k-', 'LineWidth', 4);
  legs{length(cfl{nic})+1} = 'Exact';
  xlim([0 1]);
  ylim([-.2 1.2]);
  xlabel('x','FontSize',36);
  ylabel('u','FontSize',36);
  leg = legend(legs, 'Location', 'Best');
  set( leg, 'FontSize', 24 );
  set( gca, 'FontSize', 24 );
end
