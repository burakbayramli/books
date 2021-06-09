%These are the system parameters
M = .5;
m = 0.2;
b = 0.1;
I = 0.006;
g = 9.8;
l = 0.3;
T = 0.002;
D = I*(M+m)+m*M*l^2;

A12 = (M+m)*m*g*l/D;
A13 = m*l*b/D;
A32 = -(m*l)^2*g/D;
A33 = -(I+m*l^2)*b/D;

A = [0 A12 A13 0;
    1 0 0 0;
    0 A32 A33 0;
    0 0 1 0 ];

B1 = -m*l/D;
B3 = (I+m*l^2)/D;

B = [B1;0;B3;0];
%We alter the parameters slightly to create the observer
M = .45;
m = 0.21;
b = 0.12;
I = 0.005;
g = 9.8;
l = 0.31;
%Then we calculate the observer matrices
D = I*(M+m)+m*M*l^2;

A12 = (M+m)*m*g*l/D;
A13 = m*l*b/D;
A32 = -(m*l)^2*g/D;
A33 = -(I+m*l^2)*b/D;

A_ob = [0 A12 A13 0;
    1 0 0 0;
    0 A32 A33 0;
    0 0 1 0 ];

B1 = -m*l/D;
B3 = (I+m*l^2)/D;

B_ob = [B1;0;B3;0];

C = [0 1 0 0;0 0 0 1];
%Initial state of the observer
ob_state = [0;0;0;1.2];
y_ob = C*ob_state;
%Initial state of the system
state = [0;0;0;1];
y = C*state;
measure_error = y - y_ob;

K = LQR(A_ob,B_ob,5*eye(4),5,0)
system_eig = eig(A_ob-B_ob*K)

%We set the observer eigen values to be ten times larger than the system
%eigen values to make sure the observer converges faster than the system.
%we do that with the parameter "rho"
rho = 5;
%We do the multiplication only on the real part, that is responsible for the convergenece rate.
obs_eig = rho*real(system_eig)
%Then we add the imaginary part that is responsible for the oscilation
%frequency
obs_eig = obs_eig + imag(system_eig);
L = place(A_ob',C',obs_eig')

L = L';

%K = [-3.9302 -20.3672 -2.0408 -1.000];
%K = [-3.9302 -20.3672 -2.0408 -1.000];
state_collection = [];
ob_state_collection = [];
for i = 1:3000
    measure_error = y - y_ob;
    
    u = -K*ob_state;
    next_ob_state = (A_ob*T+eye(4))*ob_state + B_ob*T*u + T*L*measure_error;
    ob_state = next_ob_state;
    y_ob = C*ob_state;
    
    next_state = (A*T+eye(4))*state + B*T*u;
    state = next_state;
    y = C*state + 0.05*randn(2,1);
    
    state_collection = [state_collection;[i*T state' u]];
    ob_state_collection = [ob_state_collection;[i*T ob_state' u]];
end

time = state_collection(:,1);
theta_dot = state_collection(:,2);
theta = state_collection(:,3);
cart_vel = state_collection(:,4);
cart_pos = state_collection(:,5);

ob_theta_dot = ob_state_collection(:,2);
ob_theta = ob_state_collection(:,3);
ob_cart_vel = ob_state_collection(:,4);
ob_cart_pos = ob_state_collection(:,5);

u = state_collection(:,6);

%To plot the phase portrait, we calculate the errors for each state:

observation_error = state_collection(:,[2:end]) - ob_state_collection(:,[2:end]);
observation_error_rate = diff(observation_error) ./ T;
observation_error_rate = [observation_error_rate;observation_error_rate(end,:)];

figure;
plot(time,theta,'k-.',time,theta_dot,'k.',time,cart_pos,'k-',time,cart_vel,'k--');
legend('Rod angle','Rod ang. velocity','Cart position','Cart velocity');
hold on;
plot(time,ob_theta,'m-.',time,ob_theta_dot,'m.',time,ob_cart_pos,'m-',time,ob_cart_vel,'m--');
% legend('Rod angle','Rod ang. velocity','Cart position','Cart velocity');
title(['rho = ' num2str(rho)]);
xlabel('Time [sec]');

figure;
plot(time,u)
xlabel('Time [sec]');
ylabel('Control force [N]');

figure;
subplot(2,2,1)
plot(observation_error(:,1),observation_error_rate(:,1),'.');
xlabel('Error of angular velocity estimate');
ylabel('Error rate');
title(['rho = ' num2str(rho)]);

subplot(2,2,2)
plot(observation_error(:,2),observation_error_rate(:,2),'.');
xlabel('Error of angle estimate');
ylabel('Error rate');
title(['rho = ' num2str(rho)]);

subplot(2,2,3)
plot(observation_error(:,3),observation_error_rate(:,3),'.');
xlabel('Error of cart velocity estimate');
ylabel('Error rate');

subplot(2,2,4)
plot(observation_error(:,4),observation_error_rate(:,4),'.');
xlabel('Error of cart position estimate');
ylabel('Error rate');
