c = 1.0;
N = 100;
cfl = 0.45;

fic = @(x)(x<0.4).*(x>0.1)+(x<0.9).*(x>0.6).*(1-abs(x-0.75)/0.15);

x_ref = linspace(0,1,1001);

dt = 'rk2'; flx = 'osher';

lims = {'zero','minmod','superbee','mc','vanleer','vanalbada'};
spec = {'k--','b-','g-','c-','r-','m-'};

u_ref = fic(x_ref);
T = 1.0;

figure('units','normalized','outerposition',[0 0 1 1]);
hold on;
for nl = 1 : length(lims)
  [x_sol, u_sol] = adv_muscl_cauchy(N,cfl,c,dt,flx,lims{nl},fic,T);
  plot(x_sol, u_sol, spec{nl}, 'LineWidth', 4, 'MarkerSize', 12);
  legs{nl} = ['limiter=' lims{nl}];
end
plot(x_ref, u_ref, 'k-', 'LineWidth', 4);
legs{nl+1} = 'Exact';
xlim([0 1]);
ylim([-.2 1.5]);
xlabel('x','FontSize',36);
ylabel('u','FontSize',36);
leg = legend(legs, 'Location', 'NE'); %'Best');
set( leg, 'FontSize', 24 );
set( gca, 'FontSize', 24 );
