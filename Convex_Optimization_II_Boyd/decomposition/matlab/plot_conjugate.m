% first plot the cost function phi(x) = x/(1-x) for 0 < x < c
% where link capacities are c = 1
x = 0:0.01:1-0.01;
phi_x = x./(1-x);

figure(1), clf
plot(x,phi_x,'k-','LineWidth',2);
hold on;
plot([0 0],[0 100],'k-', [1 1],[0 100],'b--','LineWidth',1.5);
axis([-0.3 1.3 0 8]);
set(gca, 'FontSize',18,'XTick',[0 .5 1],'YTick',[0 2 4 6 8]);
xlabel('x');
ylabel('phi');
print -depsc queue_delay_cost.eps

% then plot its conjugate function
y_left = -10:0.1:1;
conj_left = zeros(size(y_left));
y_right = 1:0.1:10;
conj_right = (sqrt(y_right)-1).^2;
y = [ y_left y_right ];
phi_conj = [ conj_left conj_right ];

figure(2), clf
plot(y,phi_conj,'k-','LineWidth',2);
hold on;
plot([1 1],[-1 10],'b--');
axis([-2 6 -0.1 2.1]);
set(gca, 'FontSize',18);
xlabel('y');
ylabel('phi*');
print -depsc queue_delay_conj.eps

% finally plot the nonlinear characteristic of the resistor (Zener diode)
dv_left = -20:0.1:1;
current_left = zeros(size(dv_left));
dv_right = 1:0.1:20;
current_right = 1-1./sqrt(dv_right);
dv = [ dv_left dv_right ];
current = [ current_left current_right ];

figure(3), clf
plot([-20 20],[0 0],'b--');
hold on;
plot([1 1],[-2  2],'b--');
plot(dv, current, 'k-','LineWidth',2);
axis([-5 10 -.5 1]);
set(gca, 'FontSize',18);
xlabel('dv');
ylabel('current');
print -depsc voltage_current_plot.eps
