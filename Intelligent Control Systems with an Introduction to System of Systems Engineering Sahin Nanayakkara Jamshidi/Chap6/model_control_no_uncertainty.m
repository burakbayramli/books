T = 0.002; %Sampling time interval
f = 0.2; % Frequency of the desired trajectory
Time_span = 1000; % The number of sampling points in the trajectory
format long;
thetad = []; %We collect the 
for i = 1:Time_span
theta_d1 = pi/6 + pi/4*sin(2*pi*f*T*i - pi/6); %Desired trajectory of joint-1
theta_d2 = -pi/6 + pi/6*sin(2*pi*f*T*i + pi/6); %Desired trajectory of joint-2
thetad = [thetad;[theta_d1 theta_d2]]; % We collect the desired trajectory data like this
end

theta_dot_d = diff(thetad) ./ T; % Differentiate the trajectory to derive the joint velocities
theta_ddot_d = diff(theta_dot_d) ./ T; % The joint accelerations

theta_dot_d = [theta_dot_d;repmat(theta_dot_d(end,:),(Time_span-length(theta_dot_d)),1)]; % We copy the last row to fill up
theta_ddot_d = [theta_ddot_d;repmat(theta_ddot_d(end,:),(Time_span-length(theta_ddot_d)),1)];

%We comment the following block that contains the parameters of the controller here
%because we use the parameters of the exact dynamic model of the
%manipulator in the controller.

% cont_iner_para.p1 = 0.027;
% cont_iner_para.p2 = 0.0044;
% cont_iner_para.p3 = 0.0011;
% cont_iner_para.p4 = 0.0022;
% cont_iner_para.p5 = 0.0017;
% 
% cont_coriolis_para.p1 = 0.0022;
% cont_coriolis_para.p2 = 0.0795;
% 
% cont_gravity_para.p1 = 0.0795;
% cont_gravity_para.p2 = 0.2978;
% cont_gravity_para.p3 = 0.0795;

% Parameters of the robot's dynamic model 
iner_para.p1 = 0.0363;
iner_para.p2 = 0.006;
iner_para.p3 = 0.00135;
iner_para.p4 = 0.003;
iner_para.p5 = 0.0023;

coriolis_para.p1 = 0.003;
coriolis_para.p2 = 0.0981;

gravity_para.p1 = 0.0981;
gravity_para.p2 = 0.8829;
gravity_para.p3 = 0.0981;

tau_data = [];
state_data = [];
% We initialize the state of the manipulator at the initial desired state.

state = [thetad(1,1),thetad(1,2),theta_dot_d(1,1),theta_dot_d(1,2)]';
for i = 1:Time_span
    M_cont = inertia(iner_para,thetad(i,1),thetad(i,2));
    V_cont = coriolis(coriolis_para,thetad(i,1),thetad(i,2),theta_dot_d(i,1),theta_dot_d(i,2));
    G_cont = gravity(gravity_para,thetad(i,1),thetad(i,2));
    tau = torque(theta_ddot_d(i,1),theta_ddot_d(i,2),M_cont,V_cont,G_cont);
       
    tau_data = [tau_data;[T*i,tau']];
    
    state = model(iner_para,coriolis_para,gravity_para,tau,state,T);
    
    state_data = [state_data;[T*i state' thetad(i,1),thetad(i,2)]];
    
end

time = state_data(:,1);
theta1 = state_data(:,2);
theta2 = state_data(:,3);
theta_dot1 = state_data(:,4);
theta_dot2 = state_data(:,5);
subplot(2,1,1)
plot(time,theta1,'O',time,thetad(:,1),'k-',time,theta2,'d',time,thetad(:,2),'k--');

ylabel('Joint angles [rad]');
legend('theta1','thetad1','theta2','thetad2');
subplot(2,1,2)
plot(time,(thetad(:,1)-theta1),'k-',time,(thetad(:,2)-theta2),'k--');

ylabel('Angular error [rad]');
legend('Joint-1','Joint-2');
xlabel('Time [sec]');

figure;
plot(tau_data(:,1),tau_data(:,2),'k-',tau_data(:,1),tau_data(:,3),'k--');

legend('Joint-1 torque [Nm]','Joint-2 torque [Nm]');
ylabel('Joint torques [Nm]');
xlabel('Time [sec]');