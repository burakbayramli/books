N = 100;
cfl = 0.45;
vars = 'prmv';

lims = {'zero','minmod','superbee','mc','vanleer','vanalbada'};
spec = {'k--','b-','g-','c-','r-','m-'};

N_ref = 1000;

% Compute the numerical solutions
for nl = 1 : length(lims)
  [x, h, u] = swe_riemann_muscl(N,cfl,vars,lims{nl});
  x_sol{nl} = x;
  h_sol{nl} = h;
  u_sol{nl} = u;
end
[x_ref, h_ref, u_ref] = swe_riemann_ref(N_ref);

% Plot height
figure('units','normalized','outerposition',[0 0 1 1]);
hold on;
for nl = 1 : length(lims)
  plot(x_sol{nl}, h_sol{nl}, spec{nl}, 'LineWidth', 4, 'MarkerSize', 12);
  legs{nl} = ['limiter=' lims{nl}];
end
plot(x_ref, h_ref, 'k-', 'LineWidth', 4, 'MarkerSize', 12);
legs{nl+1} = 'exact';
%xlim([0 1]);
%ylim([-.2 1.5]);
xlabel('x','FontSize',36);
ylabel('height','FontSize',36);
leg = legend(legs, 'Location', 'Best');
set( leg, 'FontSize', 24 );
set( gca, 'FontSize', 24 );

% Plot velocity
figure('units','normalized','outerposition',[0 0 1 1]);
hold on;
for nl = 1 : length(lims)
  plot(x_sol{nl}, u_sol{nl}, spec{nl}, 'LineWidth', 4, 'MarkerSize', 12);
  legs{nl} = ['limiter=' lims{nl}];
end
plot(x_ref, u_ref, 'k-', 'LineWidth', 4, 'MarkerSize', 12);
legs{nl+1} = 'exact';
%xlim([0 1]);
%ylim([-.2 1.5]);
xlabel('x','FontSize',36);
ylabel('velocity','FontSize',36);
leg = legend(legs, 'Location', 'Best');
set( leg, 'FontSize', 24 );
set( gca, 'FontSize', 24 );
