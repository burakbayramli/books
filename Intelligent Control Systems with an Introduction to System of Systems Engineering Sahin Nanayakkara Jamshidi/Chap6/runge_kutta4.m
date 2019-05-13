function runge_kutta4(a, b, N, alpha)

%function runge_kutta4() approximates the solution of the IVP
%y' = f(t,y) with t in the interval [a;b] and y(a) = alpha (the initial condition)
%by the Runge-Kutta method of order four.

%the inputs to this function are:
%a and b: the endpoints of the interval where the solution is to be approximated
%N: the number of subdivisions in the interval [a;b]
%alpha: the initial condition
%the ouput is a set of values approximating the solution of the IVP
%some extra lines of code could be added for visualization and comparison with the exact 
%solution if known [plot(t, exact solution as y(t)]

%This file was generated after following a course in numerical methods at
%the University of Pretoria, Department of Mathematics and Applied Mathematics
%The algorithmic scheme in this file was drawn from the book of Burden & Faires
%Numerical Analysis, 7th Ed.

%Author: Alain G. Kapitho
%Date  : Dec. 2005


h = (b-a)/N;		%the step size
t(1) = a;			
w(1) = alpha;		%the initial value

for i = 1:N
   k1 = h*f(t(i), w(i));
   k2 = h*f(t(i)+h/2, w(i)+(k1)/2);
   k3 = h*f(t(i)+h/2, w(i)+(k2)/2);
   k4 = h*f(t(i)+h, w(i)+k3);
   w(i+1) = w(i) + (k1 + 2*k2 + 2*k3 + k4)/6;
   t(i+1) = a + i*h;
end

[t' w']

%this line of code below is optional if visualization is not needed
plot(t, w, 'r*')


%function relating the right-hand side of the differential equation
%it has to be changed accordingly to the problem at hand
%in this case, the differential equation is: y' = y/t - (y/t)^2
function dy = f(t, y)

dy = y./t - (y./t)^2;

