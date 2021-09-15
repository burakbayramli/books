% generate_phase_plots_ex.m
% This routine generates a phase space plot
% for the 2-D nonlinear ODE system with an
% unstable steady state at [1,1] and a
% second stable steady state nearby. The
% function unstable_calc_f returns the
% function vector for this system.
% Written by
% Kenneth J. Beers
% MIT Department of Chemical Engineering
% March 22, 2005

function iflag = generate_phase_plots_ex();
iflag = 0;

% set number of trajectories to compute
N_traj = 200;
% set the end time of each simulation
t_end = 2;
% set the values of the steady state
x_s1 = [1; 1];  x_s2 = [-2.1761; 1.5595];
% set center of plot
x_center = 0.5*(x_s1+x_s2);
% Set the Jacobian at each steady state
% steady state # 1
Jac_s1 = zeros(2,2);
Jac_s1(1,1) = 2*x_s1(1) + 1;  Jac_s1(1,2) = -1;
Jac_s1(2,1) = -1;  Jac_s1(2,2) = -6*x_s1(2) + 2;
% steady state # 2
Jac_s2 = zeros(2,2);
Jac_s2(1,1) = 2*x_s2(1) + 1;  Jac_s2(1,2) = -1;
Jac_s2(2,1) = -1;  Jac_s2(2,2) = -6*x_s2(2) + 2;
% compute the eigenvalues and eigenvectors of the Jacobians
[W_s1,D_s1] = eig(Jac_s1);
[W_s2,D_s2] = eig(Jac_s2);

% make a figure and plot the two steady states as diamonds
figure;  plot(x_s1(1),x_s1(2),'kd');
hold on;  plot(x_s2(1),x_s2(2),'kd');

% plot stable and unstable manifolds of each steady state
% steady state # 1
s_vect = [-4:1:4];
for k=1:2
    x1_vect = x_s1(1) + s_vect*W_s1(k,1);
    x2_vect = x_s1(2) + s_vect*W_s1(k,2);
    if(D_s1(k,k) < 0)  % stable eigenvalue
        plot(x1_vect,x2_vect,'k');
    else  % unstable eigenvalue
        plot(x1_vect,x2_vect,'k--');
    end
end
% steady state # 2
for k=1:2
    x1_vect = x_s2(1) + s_vect*W_s2(k,1);
    x2_vect = x_s2(2) + s_vect*W_s2(k,2);
    if(D_s2(k,k) < 0)  % stable eigenvalue
        plot(x1_vect,x2_vect,'k');
    else  % unstable eigenvalue
        plot(x1_vect,x2_vect,'k--');
    end
end
        
% Now, compute the trajectory starting from different
% initial states in the system
for k = 1:N_traj
    % set initial state with small perturbation
    x_0 = [-4;-4] + 8*rand(2,1);
    plot(x_0(1),x_0(2),'o');

    % compute response, turning off when we get too far away
    % from initial state
    [t_traj,x_traj] = ode45(@unstable_calc_f_2,[0 t_end],x_0);

    % add trajectory to phase plot
    plot(x_traj(:,1),x_traj(:,2));
end
% add labels and title
xlabel('x_1');  ylabel('x_2');
title('Phase plots with stable and unstable steady states');
% set limits
axis([-4 4 -4 4]);

% - - - - -
% Now, compute domain of attraction for the stable steady state.
% set the initial guess values
x1_0 = [-4:0.2:4];
x2_0 = [-4:0.2:4];
[X1_g0,X2_g0] = meshgrid(x1_0,x2_0);

% allocate memory to store a 1 for those
% points within the domain of attraction
IS_IN_DA = zeros(size(X1_g0));

% for each point, compute the trajectory and see
% if it ends up near to the desired stable steady
% state
atol = 0.1;
t_end = 50;
Nx1 = size(X1_g0,1);  Nx2 = size(X1_g0,2);
for kx1 = 1:Nx1
    disp(['Passing kx1 = ', int2str(kx1), ...
        ' out of ', int2str(length(x1_0))]);
    for kx2 = 1:Nx2
        % set the initial guess
        x1 = X1_g0(kx1,kx2);
        x2 = X2_g0(kx1,kx2);
        x_0 = [x1; x2];
        % compute the trajectory
        [t_traj,x_traj] = ode45(@unstable_calc_f_2,...
            [0 0.5*t_end t_end],x_0);
        % check if final point is near stable steady
        % state
        x_f = x_traj(3,:)';
        dx = x_f - x_s2;
        dist = norm(dx,2);
        if(dist<atol)
            IS_IN_DA(kx1,kx2) = 1;
        end
    end
end
contour(X1_g0,X2_g0,IS_IN_DA,1,'r-.');

iflag = 1;
return;


% ------------------------------
% unstable_calc_f_2.m
function f = unstable_calc_f_2(t,x);

f = zeros(2,1);
f(1) = x(1)^2 + x(1) - x(2) - 1;
f(2) = -x(1) - 3*x(2)^2 + 2*x(2) + 2;
if(norm(x,inf) > 100)
    f = [0;0];
end

return;
