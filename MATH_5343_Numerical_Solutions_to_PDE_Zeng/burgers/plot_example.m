N = 100; cfl = 0.8;
% % First Riemann problem - FDM
% [x_fdm, t_fdm, U_fdm] = burgers_fdm(N,cfl,-.5,1.5,2,1.0,0.0);
% figure('units','normalized','outerposition',[0 0 1 1]);
% surf(x_fdm, t_fdm, U_fdm);
% colormap('gray');
% alpha 0.8;
% xlim([-.5 1.5]);
% ylim([0 2]);
% zlim([-.1 1.1]);
% xlabel('x','FontSize',36);
% ylabel('t','FontSize',36);
% zlabel('u','FontSize',36);
% set( gca, 'FontSize', 24 );
% view([6 48]);
% % First Riemann problem - FVM
% [x_fvm, t_fvm, U_fvm] = burgers_fvm(N,cfl,-.5,1.5,2,1.0,0.0);
% figure('units','normalized','outerposition',[0 0 1 1]);
% surf(x_fvm, t_fvm, U_fvm);
% colormap('gray');
% alpha 0.8;
% xlim([-.5 1.5]);
% ylim([0 2]);
% zlim([-.1 1.1]);
% xlabel('x','FontSize',36);
% ylabel('t','FontSize',36);
% zlabel('u','FontSize',36);
% set( gca, 'FontSize', 24 );
% view([6 48]);

% % Second Riemann problem - FDM
% [x_fdm, t_fdm, U_fdm] = burgers_fdm(N,cfl,-.5,2.0,1,2.0,1.0);
% figure('units','normalized','outerposition',[0 0 1 1]);
% surf(x_fdm, t_fdm, U_fdm);
% colormap('gray');
% alpha 0.8;
% xlim([-.5 2]);
% ylim([0 1]);
% zlim([.9 2.1]);
% xlabel('x','FontSize',36);
% ylabel('t','FontSize',36);
% zlabel('u','FontSize',36);
% set( gca, 'FontSize', 24 );
% view([6 48]);
% % First Riemann problem - FVM
% [x_fvm, t_fvm, U_fvm] = burgers_fvm(N,cfl,-.5,2.0,1,2.0,1.0);
% figure('units','normalized','outerposition',[0 0 1 1]);
% hold on;
% surf(x_fvm, t_fvm, U_fvm);
% %plot3([1.5 1.5], [0 1], [1 1], 'r-', 'LineWidth', 4);
% colormap('gray');
% alpha 0.8;
% xlim([-.5 2]);
% ylim([0 1]);
% zlim([.9 2.1]);
% xlabel('x','FontSize',36);
% ylabel('t','FontSize',36);
% zlabel('u','FontSize',36);
% set( gca, 'FontSize', 24 );
% view([6 48]);

% First Riemann problem - FDM
[x_fdm, t_fdm, U_fdm] = burgers_fdm(N,cfl,-.5,1.5,1,0.0,1.0);
figure('units','normalized','outerposition',[0 0 1 1]);
surf(x_fdm, t_fdm, U_fdm);
colormap('gray');
alpha 0.8;
xlim([-.5 1.5]);
ylim([0 1]);
zlim([-.1 1.1]);
xlabel('x','FontSize',36);
ylabel('t','FontSize',36);
zlabel('u','FontSize',36);
set( gca, 'FontSize', 24 );
view([6 48]);
% First Riemann problem - FVM
[x_fvm, t_fvm, U_fvm] = burgers_fvm(N,cfl,-.5,1.5,1,0.0,1.0);
figure('units','normalized','outerposition',[0 0 1 1]);
surf(x_fvm, t_fvm, U_fvm);
colormap('gray');
alpha 0.8;
xlim([-.5 1.5]);
ylim([0 1]);
zlim([-.1 1.1]);
xlabel('x','FontSize',36);
ylabel('t','FontSize',36);
zlabel('u','FontSize',36);
set( gca, 'FontSize', 24 );
view([6 48]);
