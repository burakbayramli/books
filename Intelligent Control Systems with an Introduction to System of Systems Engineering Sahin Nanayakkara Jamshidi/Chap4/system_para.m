function [AA,BB,H,P_k_apriori,x_k_apriori,Q,R,feed_back] = system_para(T)
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
%Model of the dynamic system X(k+1) = (I+TA)X(k) + TBu(k)
A = [0 A12 A13 0;
    1 0 0 0;
    0 A32 A33 0;
    0 0 1 0 ];

B1 = -m*l/D;
B3 = (I+m*l^2)/D;
B = [B1;0;B3;0];
%Measurement model
H = [0 1 0 0;0 0 0 1];
size_input_vector = 1;
%Optimum linear quadratic regulator feedback gains
feed_back = LQR(A,B,5*eye(length(A)),5*eye(length(size_input_vector)),0);

%Descrete model X(k+1) = AA X(k) + BB u(k)
AA = eye(length(A)) + T*A;
BB = B*T;
%Initial error covariance
P_k_apriori = 0.0*eye(4);
%Initial system state
x_k_apriori = [0 0 0 1.2]';
%Initial process noise covariance
Q = eye(4);
%Initial measurement noise covariance
R = eye(2);

