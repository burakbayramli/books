function Foguth_HW5_P1
%Last updated on 16 Oct 2013 by Lucas C. Foguth
%This function calls various other functions which employ implicit Euler to
%solve two systems of DAEs, one which is equivalent to the ODE system in
%Problem 1, and one which includes the effects of temeprature in an
%adiabtic batch reactor
%INPUTS: NA
%OUTPUTS: 6 plots, showing state tracetories, local truncation error,
%global truncation error, and computation time for the isothermal case, and
%state trajectories (concentrations and temperature) for the adiabatic
%case.

%Clearing everything
clear
clc

%Simulation time span
%[start time; finish time]
tspan = [0; 10];

%Time step size
stepsize = logspace(-4,1,6);

%Vector of initial concentrations
%[C_A; C_B; C_C]
y0 = [1; 0; 0];

%initialize vectors of computation time and local truncation error
times = zeros(2,6);
loc_err = zeros(length(stepsize));

%looping through the different time step sizes
for i = 1:6
    
    %Find the analytical solution so that we can calculate the local
    %truncation error
    anal_sol_loc = [exp(-1000*stepsize(i)); (999/998-1/2)*exp(-2*stepsize(i))-999*exp(-1000*stepsize(i))/998+0.5; 0];
    anal_sol_loc(3) = 1  - anal_sol_loc(1) - anal_sol_loc(2);
    
    %Simulate the ODE-IVP using Implicit Euler
    tic
    [T,Y] = implicit_euler(@batch_sys, tspan, stepsize(i), y0);
    times(1,i) = toc;
    
    %Simulate the ODE-IVP using Implicit Euler using a smarter method that
    %takes advantage of the linearity of the system
    tic
    [T,Y] = implicit_smart(@batch_sys, tspan, stepsize(i), y0);
    times(2,i) = toc;
    %plot the state trajcetories
    figure(1)
    subplot(3,2,i)
    plot(T,Y(1,:),'r',T,Y(2,:),'b',T,Y(3,:),'k')
    title(['h = 1E' num2str(log10(stepsize(i)))])
    xlabel('Time (s)')
    ylabel('Concentration (M)')
    %Calculate up all of the errors
    loc_err(i) = norm(Y(:,2)-anal_sol_loc(:));
    for j = 1:length(T)
        anal_sol_glob(:,j) = [exp(-1000*T(j)); (999/998-1/2)*exp(-2*T(j))-999*exp(-1000*T(j))/998+0.5; 0];
        anal_sol_glob(3,j) = 1  - anal_sol_glob(1,j) - anal_sol_glob(2,j);
        error(:,j) = Y(:,j) - anal_sol_glob(:,j);
        error1(j) = norm(error(:,j),1);
        error2(j) = norm(error(:,j),2);
        errorinf(j) = norm(error(:,j),inf);
    end
    glob_err_1(i) = norm(error1,1);
    glob_err_2(i) = norm(error2,2);
    glob_err_inf(i) = norm(errorinf,inf);
    
end%for loop

%Plot up the local truncation errors
figure(2)
loglog(stepsize,loc_err,'rx-')
xlabel('\Delta t')
ylabel('E')
title('Local Error')

%Plot up the global truncation errors
figure(3)
%one norm
loglog(stepsize,glob_err_1,'rx-')
hold on
%two norm
loglog(stepsize,glob_err_2,'ro-')
%inf norm
loglog(stepsize,glob_err_inf,'r+-')
xlabel('\Delta t')
ylabel('E')
title('Global Error')
legend('1-norm','2-norm','inf-norm')

%Plot up the computational time
figure(4)
loglog(stepsize,times(1,:),'r',stepsize,times(2,:))
ylabel('Comp. time (s)')
xlabel('Step size (s)')
legend('fsolve','backslash')

%Now we'll consider the adiabatic case
%Simulate the ODE-IVP using Implicit Euler
%Time step size
stepsize = 1E-5;
%vector of initial conditions
y0 = [1; 0; 0; 300];
%Do the actual simulation
[T,Y] = implicit_euler(@batch_sys2, tspan, stepsize, y0);
%Plot up the concentration trajectories
figure(5)
plot(T,Y(1,:),'r',T,Y(2,:),'b',T,Y(3,:),'k')
legend('C_A','C_B','C_C')
xlabel('Time (s)')
ylabel('Concentration (M)')
%Plot up the temperature profile
figure(6)
plot(T,Y(4,:))
xlabel('Time (s)')
ylabel('Temperature (K)')

end%main

function [T,Y] = implicit_euler(fun_handle, tspan, h, y0)
%This function implements the implicit Euler method to solve a system of
%DAEs
%INPUTS: fun_handle, a handle to a function which returns the time
%derivatives, tspan, a vector containing start and stop times, h, a
%time step size, and y0, the initial condition vector
%OUTPUTS: T, a vector of timepoints, and Y, a matrix of results

%Make sure fsolve doesn't spew info at us
options = optimset('Display','none','TolFun',1e-14,'TolX',1e-14);

i = 1; %initial index
T(i) = tspan(1); %initial time
Y(:,i) = y0; %initial value
while T(i) < tspan(2)
    T(i+1) = T(i) + h; %update time
    Y(:,i+1) = fsolve(@(x) fun_handle(x,Y(:,i),y0,h),Y(:,i),options); %update value
    i = i + 1; %update index
end%while loop

end%function

function [T,Y] = implicit_smart(~, tspan, h, y0)
%This function implements the implicit Euler method to solve a system of
%DAEs
%INPUTS: fun_handle, a handle to a function which returns the time
%derivatives, tspan, a vector containing start and stop times, h, a
%time step size, and y0, the initial condition vector
%OUTPUTS: T, a vector of timepoints, and Y, a matrix of results

k1 = 1000;%s^{-1}
k2 = 1;%s^{-1}
k3 = 1;%s^{-1}

i = 1; %initial index
T(i) = tspan(1); %initial time
Y(:,i) = y0; %initial value
while T(i) < tspan(2)
    T(i+1) = T(i) + h; %update time
    A = [-k1*h-1, 0, 0; h*k1, -k2*h-1, h*k3; 1, 1, 1];
    Y(:,i+1) = A\[-Y(1:2,i);sum(y0)];
    %Y(:,i+1) = fsolve(@(x) Y(:,i) + h*fun_handle(x) - x,Y(:,i),options); %update value
    i = i + 1; %update index
end%while loop

end%function

function y = batch_sys(x,C,c0,h)
%This function takes in a vector of concentrations C = [C_A; C_B; C_C] and
%applies finite differences (implicit Euler) to the system of interest,
%producting a vector which should be zero when a solution to the system of
%equations is found.
%INPUTS: C, a vector containing the concentration of three species [C_A; C_B; C_C]
%OUTPUTS: y, a vector of "errors" which should be zero when the system of
%equations implementing implicit Euler is solved.

%Define reaction rate constants
k_1 = 1000;%s^{-1}
k_2 = 1;%s^{-1}
k_3 = 1;%s^{-1}

%Initialize vector of time derivatives
y = zeros(3,1);

%Fill in vector of time derivatives
y(1) = (x(1)-C(1))/h + k_1*x(1);
y(2) = (x(2)-C(2))/h - k_1*x(1) + k_2*x(2) - k_3*x(3);
y(3) = sum(c0) - x(1) - x(2) - x(3);

end%function

function y = batch_sys2(x,C,c0,h)
%This function takes in a vector of concentrations C = [C_A; C_B; C_C; T] and
%applies finite differences (implicit Euler) to the system of interest,
%producting a vector which should be zero when a solution to the system of
%equations is found.  This system is adiabatic and takes into account
%temperature effects.
%INPUTS: C, a vector containing the concentration of three species and the 
%temperature [C_A; C_B; C_C; T]
%OUTPUTS: y, a vector of "errors" which should be zero when the system of
%equations implementing implicit Euler is solved.

%Define reaction rate constants
k_1 = 150148.8259*exp(-1.25E4/8.314/x(4));%s^{-1}
k_2 = 461.3843455*exp(-1.53E4/8.314/x(4));%s^{-1}
k_3 = 461.3843455*exp(-1.53E4/8.314/x(4));%s^{-1}

%Initialize vector of time derivatives
y = zeros(3,1);

%Fill in vector of time derivatives
y(1) = (x(1)-C(1))/h + k_1*x(1);
y(2) = (x(2)-C(2))/h - k_1*x(1) + k_2*x(2) - k_3*x(3);
y(3) = c0(1) + c0(2) + c0(3) - x(1) - x(2) - x(3);

%Find the terms in the energy balance
CpA = x(1)*(26.63*(x(4) - c0(4)) + 0.183 *(x(4)^2 - c0(4)^2)/2 - 45.86E-6 * (x(4)^3 - c0(4)^3)/3);
CpB = x(2)*(25.04*(x(4) - c0(4)) + 0.098 *(x(4)^2 - c0(4)^2)/2 - 30.95E-6 * (x(4)^3 - c0(4)^3)/3);
CpC = x(3)*(20.47*(x(4) - c0(4)) + 0.154 *(x(4)^2 - c0(4)^2)/2 - 41.85E-6 * (x(4)^3 - c0(4)^3)/3);
Hr1 = 5.7E2*(c0(1) - x(1));
Hr2 = -1.7E2*(c0(3) - x(3));

%Implement the energy balance.
y(4) = CpA + CpB + CpC - Hr1 - Hr2;

end%function



