%Written by Thrishantha Nanayakkara
% For the book: Intelligent Systems with an Introduction to System of
% Systems Control
% The goal of this controller is to accelerate a mass to a desired
% velocity. The performance of the controller is reflected in the size of
% the control command needed, the overshoot, time to settle, and the steady
% state error.

M = 2; % The mass is 2kg
desired_vel = 4; % The desired velocity is 4 m/second
Gain = [2 1 0;
    2 2 0;
    3 0.5 0;
    4 1 0]; % A set of gain vectors we are going to try
M_est = [1.5;
    2.0;
    2.5;
    3];
%[P_gain D_gain I_gain] The integral gain: this is not used in many industrial applications due to stability issues
T = 0.1; % Sampling interval is 10 msec
% The problem: what force or sequence of forces will achieve this?
for gain_index = 1:4
    %First we study a conventional PID controller often found in the industry
    vel = 0; %Inititial velocity is zero
    data = [];
    M_estimated = M_est(gain_index,1);
    
    vel_error = desired_vel - vel; %Velocity error
    
    for index = 1:100
        prev_vel_error = vel_error;
        vel_error = desired_vel - vel; %Velocity error
        
        force = M_estimated*vel_error;
   
        accel = force/M;
        vel = vel + accel*T;
        data = [data;[index*T vel vel_error force]];
    end
    
    Time = data(:,1);
    velocities = data(:,2);
    vel_errors = data(:,3);
    figure(1)
    subplot(4,1,gain_index);
    plot(Time,velocities,'k-',Time,vel_errors,'m-');
    title(['Estimated Mass: ' num2str(M_est(gain_index,1))]);
    if gain_index == 1
        legend('Velocity','Vel error');
    end
    if gain_index == 4
        xlabel('Time [sec]');
        ylabel('Velocity/velocity error [m/sec]');
    end
    hold on;
    
    Force = data(:,4); 
    
    figure(2)
    subplot(4,1,gain_index);
    plot(Time,Force,'k-');
     title(['Estimated Mass: ' num2str(M_est(gain_index,1))]);
    if gain_index == 4
        xlabel('Time [sec]');
        ylabel('Force [N]');
    end
    
    hold on;
    
end
