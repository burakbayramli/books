nu = 0.01;
N = 50;
cfl = [0.99 10.0 100.0];
cfl_spec = {'kx--','k*-.','ks:'};

[x_ref, u_ref] = heat_cauchy_sol(1000,3,nu);

dt = 'be';

figure('units','normalized','outerposition',[0 0 1 1]);
hold on;

for m = 1 : length(cfl)
  [x_sol, u_sol] = heat_cauchy(N,cfl(m),nu,dt,'cd');
  x{m} = x_sol;
  u{m} = u_sol;
  plot(x_sol, u_sol, cfl_spec{m}, 'LineWidth', 4, 'Markersize', 12);
  legs{m} = sprintf('cfl=%f',cfl(m));
end
plot(x_ref, u_ref, 'k-', 'LineWidth', 4', 'MarkerSize', 12);
legs{length(cfl)+1} = 'reference';
xlim([0 1]);
ylim([-.2 .9]);
xlabel('x','FontSize',36);
ylabel('u','FontSize',36);
leg = legend(legs, 'Location', 'Best');
set( leg, 'FontSize', 24 );
set( gca, 'FontSize', 24 );
