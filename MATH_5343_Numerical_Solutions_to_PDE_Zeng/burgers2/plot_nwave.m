N = 100; cfl = 0.8; T = 2.8;

fic = @(x)sin(2*pi*x);
% Roe
[x, u] = burgers_cauchy(N,cfl,'Roe',fic,T);
[x_ref, u_ref] = burgers_cauchy(800,cfl,'Roe',fic,T);
figure('units','normalized','outerposition',[0 0 1 1]);
hold on;
plot(x,u,'k*--','LineWidth',4,'MarkerSize',12);
plot(x_ref,u_ref,'k-','LineWidth',4);
%leg = legend('Roe method', 'Reference', 'Location', 'Best');
leg = legend('Roe method', 'Reference', 'Location', 'SouthEast');
xlim([0 1]);
ylim([-0.2 0.2]);
xlabel('x','FontSize',36);
ylabel('u','FontSize',36);
set( leg, 'FontSize', 24 );
set( gca, 'FontSize', 24 );

% Godunov
[x, u] = burgers_cauchy(N,cfl,'Godunov',fic,T);
[x_ref, u_ref] = burgers_cauchy(800,cfl,'Godunov',fic,T);
figure('units','normalized','outerposition',[0 0 1 1]);
hold on;
plot(x,u,'k*--','LineWidth',4,'MarkerSize',12);
plot(x_ref,u_ref,'k-','LineWidth',4);
%leg = legend('Godunov method', 'Reference', 'Location', 'Best');
leg = legend('Godunov method', 'Reference', 'Location', 'SouthEast');
xlim([0 1]);
ylim([-0.2 0.2]);
xlabel('x','FontSize',36);
ylabel('u','FontSize',36);
set( leg, 'FontSize', 24 );
set( gca, 'FontSize', 24 );

