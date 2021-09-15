% test_simulated_annealing.m
% This MATLAB m-file tests the operation of
% the simulated annealing routine.
% K. Beers. MIT ChE. 8/2/03

function iflag_main = test_simulated_annealing();

iflag_main = 0;
func_name = 'global_min_cost_func';
figure;

ModelParam.c1 = 10; ModelParam.c2 = 0.5;
ModelParam.xmin1 = zeros(2,1);
ModelParam.xmin1(1) = -3; ModelParam.xmin1(2)= 3;
ModelParam.xmin2 = zeros(2,1);
ModelParam.xmin2(1) = 3; ModelParam.xmin2(2) = -3;

% get local minimum values
Fmin1 = feval(func_name,ModelParam.xmin1,ModelParam);
disp('local minimum # 1 :');
disp(['(', num2str(ModelParam.xmin1(1)), ',', ...
        num2str(ModelParam.xmin1(2)),'), F = ', num2str(Fmin1)]);
Fmin2 = feval(func_name,ModelParam.xmin2,ModelParam);
disp('local minimum # 2 :');
disp(['(', num2str(ModelParam.xmin2(1)), ',', ...
        num2str(ModelParam.xmin2(2)),'), F = ', num2str(Fmin2)]);


% make a contour plot of the cost function
x_val = linspace(-5,5,50);
[X1,X2] = meshgrid(x_val,x_val);
F = zeros(size(X1));
for k1=1:length(x_val)
    for k2=1:length(x_val)
        x = [X1(k1,k2); X2(k1,k2)];
        F(k1,k2) = global_min_cost_func(x,ModelParam);
    end
end
contour(X1,X2,F,30);

% Now, we ask user for initial guess
disp('Enter initial guess : ');
x0 = zeros(size(x));
x0(1) = input('x0(1) : ');
x0(2) = input('x0(2) : ');

% call the simulated annealing routine
OptParam.iter_max = 100000;
OptParam.verbose = 2500;
OptParam.temp_init = 50;
OptParam.num_runs = 1;
[x,F,iflag,x_traj] = simulated_annealing(func_name,x0,OptParam,ModelParam);

disp(' ');
disp('Final results from simulated annealing : ');
x, F,

% Now add trajectory to graph
num_frames = size(x_traj,2);
hold on;
for k=1:num_frames
    plot(x_traj(1,k),x_traj(2,k),'o');
    if(k>1)
        x1_line = [x_traj(1,k);x_traj(1,k-1)];
        x2_line = [x_traj(2,k);x_traj(2,k-1)];
        line(x1_line,x2_line);
    end
end
xlabel('x_1');
ylabel('x_2');

iflag_main = iflag;

return;

