% Newton_2D_test.m
% This MATLAB program demonstrates the convergence of Newton's
% method for a system of two equations
%  f(x1,x2) = 3*x1^3 + 4*x2^2 - 145 = 0
%  f(x1,x2) = 4*x1^2 - x2^3 + 28 = 0
% that has a real solution at (3,4).
%
% The user enters a column vector x_guess that contains the
% initial guess of the solution. In addition to a plot of the
% function norm, showing the solution, the trajectory of Newton's
% method is plotted.
% K. Beers. MIT ChE. 7/20/04

function iflag_main = Newton_2D_test(x_guess);
iflag_main = 0;

calc_f = 'Newton_2D_test_calc_f';
calc_Jac = 'Newton_2D_test_calc_Jac';

num_contours = 25;

% First, make a filled 2-D contour plot of the norm of the function vector.
x1_min = -6;  x1_max = 6;  Npts_x1 = 50;
x1_vect = linspace(x1_min,x1_max,Npts_x1);
delta_x1 = x1_vect(2)-x1_vect(1);

x2_min = -6;  x2_max = 6;  Npts_x2 = 50;
x2_vect = linspace(x2_min,x2_max,Npts_x2);
delta_x2 = x2_vect(2)-x2_vect(1);

[X1,X2] = meshgrid(x1_vect,x2_vect);

F1 = 3*X1.^3 + 4*X2.^2 - 145;
F2 = 4*X1.^2 - X2.^3 + 28;
F2norm = sqrt(F1.*F1 + F2.*F2);


% Now, make a coarse grid for the gradient plots.
num_coarse = 10;
scale_quiver = 0.5;

x1_c = linspace(x1_min,x1_max,num_coarse);
delta_x1_c = x1_c(2)-x1_c(1);

x2_c = linspace(x2_min,x2_max,num_coarse);
delta_x2_c = x1_c(2)-x2_c(1);

[X1_c,X2_c] = meshgrid(x1_c,x2_c);

F1_c = 3*X1_c.^3 + 4*X2_c.^2 - 145;
F2_c = 4*X1_c.^2 - X2_c.^3 + 28;
F2norm_c = sqrt(F1_c.*F1_c + F2_c.*F2_c);

[DF2norm_DX1,DF2norm_DX2] = gradient(F2norm_c,delta_x1_c,delta_x2_c);


% Now make contour and surface plots of 2-norm
figure;  hold on;
contour(X1,X2,F2norm,num_contours);
quiver(X1_c,X2_c,DF2norm_DX1,DF2norm_DX2,scale_quiver);
title('||f||_2');
xlabel('x_1'); ylabel('x_2');

figure;  hold on;
mesh(X1,X2,F2norm);
title('2-norm surface plot');
xlabel('x_1'); ylabel('x_2'); zlabel('||f||_2');
gtext('at lowest point (3,4), ||f||_2 = 0, so (3,4) is a solution');



% We now plot a trajectory for the Newton's method iterations.
max_iter = 100;
iter_conv = 0;
x_traj = zeros(max_iter+1,2);
x = x_guess;
k = 1;
x_traj(k,:) = x';

for j=1:max_iter    
    k = k + 1;
    
    % calculate Jacobian matrix
    Jac=zeros(2,2);
    Jac(1,1) = 9*x(1)^2;
    Jac(1,2) = 8*x(2);
    Jac(2,1) = 8*x(1);
    Jac(2,2) = -3*x(2)^2;

    % calculate function vector
    f = [0;0];
    f(1) = 3*x(1)^3 + 4*x(2)^2 - 145;
    f(2) = 4*x(1)^2 - x(2)^3 + 28;
        
    % check for convergence
    f_norm = max(abs(f));
    if(f_norm < 1.e-10)
        iter_conv = j;
        break;
    end
        
    % solve for Newton step update
    dx = Jac\(-f);
        
    % update solution estimate
    x = x + dx;
        
    % store new result in trajectory vector
    x_traj(k,:) = x';
        
end

% Make plot of trajectory over that of norm of f.    
figure;  hold on;
contour(X1,X2,F2norm,num_contours);
quiver(X1_c,X2_c,DF2norm_DX1,DF2norm_DX2,scale_quiver);
title('trajectory and ||f||_2 contour plot');
xlabel('x_1'); ylabel('x_2');

if(iter_conv)
    plot(x_traj(1,1),x_traj(1,2),'S');  hold on;
    plot(x_traj(1:iter_conv,1),x_traj(1:iter_conv,2),'-o');
    gtext(['Initial guess : x_1 = ', num2str(x_guess(1)), ...
        ', x_2 = ', num2str(x_guess(2)), ...
        '  # iter. = ', int2str(iter_conv)]);
else
    plot(x_traj(1,1),x_traj(1,2),'S');  hold on;
    plot(x_traj(:,1),x_traj(:,2),'-o');
    gtext(['Initial guess : x_1 = ', num2str(x_guess(1)), ...
        ', x_2 = ', num2str(x_guess(2)), ...
        '  unconverged']);
end



   
% ------------------------------------------------------------
% Next, use the reduced Newton's step method with the same
% convergence criterion.

Options.max_iter = max_iter;
Options.max_iter_LS = 10000;
Options.rtol = 1;
Options.atol = 1.e-10;
Options.step_tol = 1.e-3;
Options.verbose = 1;
Options.use_range = 1;
Options.range = [10; 10];
Param = 0;
x0 = x_guess;

[x,iflag,iter_conv,x_traj,f_traj] = ...
    reduced_Newton(x0,calc_f,calc_Jac,Options,Param);


% Then, make new figure with just reduced-step result
figure;  hold on;
contour(X1,X2,F2norm,num_contours);
quiver(X1_c,X2_c,DF2norm_DX1,DF2norm_DX2,scale_quiver);
xlabel('x_1'); ylabel('x_2');

if(iter_conv)
    plot(x_traj(1,1),x_traj(1,2),'S');  hold on;
    plot(x_traj(:,1),x_traj(:,2),'-o');
    title(['reduced-step: ', ...
            'Initial guess : x_1 = ', num2str(x_guess(1)), ...
        ', x_2 = ', num2str(x_guess(2)), ...
        '  # iter. = ', int2str(iter_conv)]);
else
    plot(x_traj(:,1),x_traj(:,2),'-o');
      title(['reduced-step: ', ...
              'Initial guess : x_1 = ', num2str(x_guess(1)), ...
        ', x_2 = ', num2str(x_guess(2)), ...
        '  unconverged']);
end


iflag_main = 1;
return;
