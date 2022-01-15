N = 100; cfl = 0.8;

% First Riemann problem - shock - Roe
[x, u] = burgers_riem(N,cfl,'Roe',-.5,1.5,2,1.0,0.0);
x_ref = linspace(-.5,1.5,10001);
u_ref = x_ref < 1.0;
figure('units','normalized','outerposition',[0 0 1 1]);
hold on;
plot(x,u,'k*--','LineWidth',4,'MarkerSize',12);
plot(x_ref,u_ref,'k-','LineWidth',4);
%leg = legend('Roe method', 'Reference', 'Location', 'Best');
leg = legend('Roe method', 'Reference', 'Location', 'SouthWest');
xlim([-.5 1.5]);
ylim([-.1 1.1]);
xlabel('x','FontSize',36);
ylabel('u','FontSize',36);
set( leg, 'FontSize', 24 );
set( gca, 'FontSize', 24 );

% First Riemann problem - shock - Godunov
[x, u] = burgers_riem(N,cfl,'Godunov',-.5,1.5,2,1.0,0.0);
x_ref = linspace(-.5,1.5,10001);
u_ref = x_ref < 1.0;
figure('units','normalized','outerposition',[0 0 1 1]);
hold on;
plot(x,u,'k*--','LineWidth',4,'MarkerSize',12);
plot(x_ref,u_ref,'k-','LineWidth',4);
%leg = legend('Godunov method', 'Reference', 'Location', 'Best');
leg = legend('Godunov method', 'Reference', 'Location', 'SouthWest');
xlim([-.5 1.5]);
ylim([-.1 1.1]);
xlabel('x','FontSize',36);
ylabel('u','FontSize',36);
set( leg, 'FontSize', 24 );
set( gca, 'FontSize', 24 );

% Second Riemann problem - rarefaction - Roe
[x, u] = burgers_riem(N,cfl,'Roe',-.5,1.5,1,0.0,1.0);
x_ref = linspace(-.5,1.5,10001);
u_ref = 0.0*(x_ref<0.0) + 1.0*(x_ref>1.0) + x_ref.*(x_ref>=0.0).*(x_ref<=1.0);
figure('units','normalized','outerposition',[0 0 1 1]);
hold on;
plot(x,u,'k*--','LineWidth',4,'MarkerSize',12);
plot(x_ref,u_ref,'k-','LineWidth',4);
%leg = legend('Roe method', 'Reference', 'Location', 'Best');
leg = legend('Roe method', 'Reference', 'Location', 'NorthWest');
xlim([-.5 1.5]);
ylim([-.1 1.1]);
xlabel('x','FontSize',36);
ylabel('u','FontSize',36);
set( gca, 'FontSize', 24 );

% Second Riemann problem - rarefaction - Godunov
[x, u] = burgers_riem(N,cfl,'Godunov',-.5,1.5,1,0.0,1.0);
x_ref = linspace(-.5,1.5,10001);
u_ref = 0.0*(x_ref<0.0) + 1.0*(x_ref>1.0) + x_ref.*(x_ref>=0.0).*(x_ref<=1.0);
figure('units','normalized','outerposition',[0 0 1 1]);
hold on;
plot(x,u,'k*--','LineWidth',4,'MarkerSize',12);
plot(x_ref,u_ref,'k-','LineWidth',4);
%leg = legend('Godunov method', 'Reference', 'Location', 'Best');
leg = legend('Godunov method', 'Reference', 'Location', 'NorthWest');
xlim([-.5 1.5]);
ylim([-.1 1.1]);
xlabel('x','FontSize',36);
ylabel('u','FontSize',36);
set( gca, 'FontSize', 24 );
