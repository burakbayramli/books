function [] = dalembert(c,step, finaltime, phi, nu, range)
% This function M-file will produce a series of snapshots of the solution
% to the one dimensional wave problem:  u_tt = c^2u_xx having initial
% displacement: u(x,0)=phi(x) and initial velocity u_t(x,0)=nu(x).  
% The snapshots run from t=0 to t = finaltime in increments of step.
% Input variables:  c = wave speed (from PDE), step = positive number
% indicated time step for snapshot intervals, phi, nu = initial
% displacement and velocity functions for wave, respectively,  and range =
% 4 by 1 vector of uniform axes range to use in plots.   The code is based
% on D'Alembert's solution of Theorem 12.1.
% Note:  Since quad is used within the program on the function nu, it is
% necessary that nu be constructed to accept vector inputs.
x=range(1):.1:range(2); 
sx = length(x);
%Set dimensions of subplot window
N = finaltime/step; %Number of shots
U=zeros(N,sx);
if N<=11
    N1=N+1; M=1;
elseif N>11&N<=21
 N1=ceil((N+1)/2); M=2;
else 
    N1=ceil((N+1)/3); M=3;
end
counter =1;
for t=0:step:finaltime
x1=x+c*t; x2=x-c*t;
for i=1:sx
u(i)=.5*(feval(phi, x1(i))+feval(phi, x2(i)));
if t>0, u(i)=u(i)+quad(nu, x2(i), x1(i),1e-5)/2/c; end
end
subplot(N1,M,counter)
plot(x,u)
hold on
axis([range]) %We fix a good axis range. 
counter=counter+1;
end
