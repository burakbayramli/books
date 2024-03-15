% Chapter 1 homework
% Problem 1.6
clear;clc
W = 60000; % N
b = 17; % m
scale = 0.1; % size of model
p = 15 * 101000; % N/m^2
T0 = 273;
T = 15 + T0; % K
R = 287; % KJ/kg/K
rho = p/(R*T);
muo = 0.0000171; % kg/m/s
mu = muo * (T/T0)^.75;
% Assume the viscosity if 
num = mu/rho;
rhop = 1.2256;
nup = mu/rhop;
Vm = [20 21 22 23 24];
Vp = Vm*nup*scale/num;
Lmx = [2960 3460 4000 4580 5200];
 plot(Vm,Lmx)
% title('Model test data given in Problem 1.6')
% xlabel('Speed at maximum lift, m/s'), 
% ylabel('Maximum lift, N')
Lm  = rho*Vm.^2.*scale^2.*W./(rhop*Vp.^2);
 hold on
plot(Vm,Lm,'r')
figure(2)
plot(Vm,Vp),grid
xlabel('Model speed, m/s'),ylabel('Prototype speed, m/s')