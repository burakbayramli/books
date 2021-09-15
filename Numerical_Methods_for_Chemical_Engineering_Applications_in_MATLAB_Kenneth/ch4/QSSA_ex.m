% QSSA_ex.m
% This MATLAB program simulates the
% activated kinetics of A --> B,
% showing how the system becomes stiff.
% Written by Kenneth J. Beers
% MIT Department of Chemical Engineering
% March 24, 2005

function iflag = QSSA_ex();
iflag = 0;

% set rate constants
k1_cM = 1;
k2 = input('Enter k2 : ');

% set initial conditions
cA_0 = 1;  x_0 = [cA_0;  0;  0];

% set end time
t_end = 10;

% simulate with ode45
cpu_1 = cputime;
[t_traj, x_traj] = ode45(@QSSA_ex_calc_f, [0 t_end], ...
    x_0, [], k1_cM, k2);
cpu_2 = cputime;
cpu_elapsed_ode45 = cpu_2 - cpu_1,

% simulate with ode15s
cpu_1 = cputime;
[t_traj, x_traj] = ode15s(@QSSA_ex_calc_f, [0 t_end], ...
    x_0, [], k1_cM, k2);
cpu_2 = cputime;
cpu_elapsed_ode15s = cpu_2 - cpu_1,


do_QSSA = input('Run sim. w/ QSSA (0=n, 1=y) : ');
if(do_QSSA)
    % perform simulation with QSSA using ode45
    cpu_1 = cputime;
    [t_traj_QSSA, cA_traj_QSSA] = ...
        ode45(@QSSA_ex_calc_f_QSSA, [0 t_end], ...
            cA_0, [], k1_cM, k2);
    cpu_2 = cputime;
    cpu_elapsed_ode45_QSSA = cpu_2 - cpu_1,
    cAs_traj_QSSA = cA_traj_QSSA.*k1_cM/k2;
    cB_traj_QSSA = cA_0*ones(size(t_traj_QSSA)) - ...
            cA_traj_QSSA - cAs_traj_QSSA;
    x_traj_QSSA = [cA_traj_QSSA, cAs_traj_QSSA, cB_traj_QSSA];
end

% plot results, as plot and semilogy
figure;
plot(t_traj,x_traj(:,1));  hold on;
plot(t_traj,x_traj(:,2),'--');
plot(t_traj,x_traj(:,3),'-.');
xlabel('time t');  ylabel('concentration c_j(t)');
title('Batch kinetics for A + M ==> A* + M,  A* ==> B');
legend('A','A*','B','Location','Best');
gtext(['k_1 c_M = ', num2str(k1_cM), ...
    ', k_2 = ', num2str(k2)]);

figure;
semilogy(t_traj,x_traj(:,1));  hold on;
semilogy(t_traj,x_traj(:,2),'--');
semilogy(t_traj,x_traj(:,3),'-.');
xlabel('time t');  ylabel('concentration c_j(t)');
title('Batch kinetics for A + M ==> A* + M,  A* ==> B');
legend('A','A*','B','Location','Best');
gtext(['k_1 c_M = ', num2str(k1_cM), ...
    ', k_2 = ', num2str(k2)]);

if(do_QSSA)
    % replace any zero/negative y values with eps to avoid
    % problems with semilogy
    [j_f,k_f] = find(x_traj_QSSA <= 0);
    x_traj_QSSA(j_f,k_f) = eps;
    figure;
    semilogy(t_traj_QSSA,x_traj_QSSA(:,1));  hold on;
    semilogy(t_traj_QSSA,x_traj_QSSA(:,2),'--');
    semilogy(t_traj_QSSA,x_traj_QSSA(:,3),'-.');
    xlabel('time t');  ylabel('concentration c_j(t)');
    title('Batch kinetics for A + M ==> A* + M,  A* ==> B w/ QSSA');
    legend('A','A*','B','Location','Best');
    gtext(['k_1 c_M = ', num2str(k1_cM), ...
        ', k_2 = ', num2str(k2)]);
end

iflag = 1;
return;


% --------------------------------
function f = QSSA_ex_calc_f(t, x, k1_cM, k2);

cA = x(1);  cAs = x(2);  cB = x(3);
r1 = k1_cM*cA;  r2 = k2*cAs;

f = zeros(3,1);
f(1) = -r1;
f(2) = r1 - r2;
f(3) = r2;

return;


% --------------------------------
function f = QSSA_ex_calc_f_QSSA(t, cA, k1_cM, k2);

r1 = k1_cM*cA;

f = -r1;

return;

