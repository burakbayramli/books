%Dynamic model of a two link manipulator
%Written by Thrishantha Nanayakkara
% For the book: Intelligent Systems with an Introduction to System of
% Systems Control
function next_state = model(cont_iner_para,cont_coriolis_para,cont_gravity_para,tau,state,T)

A = [0 0 1 0;0 0 0 1;0 0 0 0;0 0 0 0];
initial_state = state;
	%the step size
M = inertia(cont_iner_para,state(1),state(2));
V = coriolis(cont_coriolis_para,state(1),state(2),state(3),state(4));
G = gravity(cont_gravity_para,state(1),state(2));
k1 = (A*initial_state + [0;0;inv(M)*(tau - V - G)]);

state = initial_state + (T*k1)/2;

M = inertia(cont_iner_para,state(1),state(2));
V = coriolis(cont_coriolis_para,state(1),state(2),state(3),state(4));
G = gravity(cont_gravity_para,state(1),state(2));
k2 = (A*state + [0;0;inv(M)*(tau - V - G)]);

state = initial_state + (T*k2)/2;

M = inertia(cont_iner_para,state(1),state(2));
V = coriolis(cont_coriolis_para,state(1),state(2),state(3),state(4));
G = gravity(cont_gravity_para,state(1),state(2));
k3 = (A*state + [0;0;inv(M)*(tau - V - G)]);

state = initial_state + (T*k3);

M = inertia(cont_iner_para,state(1),state(2));
V = coriolis(cont_coriolis_para,state(1),state(2),state(3),state(4));
G = gravity(cont_gravity_para,state(1),state(2));
k4 = (A*state + [0;0;inv(M)*(tau - V - G)]);

next_state = initial_state + T*(k1 + 2*k2 + 2*k3 + k4)/6;
   
