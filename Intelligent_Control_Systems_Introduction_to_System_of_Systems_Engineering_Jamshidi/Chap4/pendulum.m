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

state = [0;0;0;1];
%[K,S,E] = LQR(A,B,5*eye(4),5,0);
%K = [-3.9302 -20.3672 -2.0408 -1.000];
K = [-3.9302 -20.3672 -2.0408 -1.000];
state_collection = [];
for i = 1:3000
    u = -K*state;
    next_state = (A*T+eye(4))*state + B*T*u;
    state = next_state;
    state_collection = [state_collection;[i*T state' u]];
end

time = state_collection(:,1);
theta_dot = state_collection(:,2);
theta = state_collection(:,3);
cart_vel = state_collection(:,4);
cart_pos = state_collection(:,5);
u = state_collection(:,6);

subplot(2,1,1)
plot(time,theta,'-.',time,theta_dot,'.',time,cart_pos,'-',time,cart_vel,'--');
legend('Rod angle','Rod ang. velocity','Cart position','Cart velocity');
title(['Feedback gains (K): ' num2str(K)]);
subplot(2,1,2)
plot(time,u)
xlabel('Time [sec]');
ylabel('Control force [N]');