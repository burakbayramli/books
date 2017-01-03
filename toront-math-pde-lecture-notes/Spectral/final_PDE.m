% [u,x,t,kk,amp] = burgers2b(u0,t_0,t_f,M,N)
%
% solves the periodic Burgers equation without viscosity:
%                u_t = u_xx + 10 H[u_x] + u^3 u_x
% with initial data u_0 with periodic boundary conditions using 
% spectral derivatives in
% space.  In the time-stepping, I treat the nonlinear term explicitly
% and the linear terms implicitly.  t_0 is the initial time, t_f is the
% final time, N is the number of mesh-points, and M is the number of
% time steps.  err is the error. it also returns the power spectrum of the
% solution, where kk is the collection of wave numbers and amp = log(f_k^2)
% is the magnitude of the kth Fourier mode.
%
% N SHOULD BE A POWER OF 2!  
%
% (It will work no matter what, but will go faster if N is a power
% of 2.)
%
function [u,x,t,kk,amp] = burgers2b(u0,t_0,t_f,M,N)

% define the mesh in space
dx = 2*pi/N;
x = 0:dx:2*pi-dx;
x = x';
kk = 0:1:(N/2-1);

% define the mesh in time
dt = (t_f-t_0)/M;
t = t_0:dt:t_f;

u(:,1) = u0;
[kk,amp(:,1)] = find_spec(u0);

% At time t_n, I use convolve.m to compute the nonlinearity (u^n)^3 u^n_x.  It
% gives me the fourier transform of u^n u^n_x.  Let's call that g^n.  And
% so in Fourier space I have the ODE.  I use an integrating factor method
% to timestep.
%
% d/dt hat{u^n}_k = hat{g^n}_k - k^2 hat{u^n}_k + 10 |k| hat{u^n}_k
%
% hat{u^{n+1}}_k - hat{u^n}_k = dt*( hat{g^n}_k +( - k^2 + 10 |k|) hat{u^{n+1}}_k)
% 
% hat{u^{n+1}}_k + (k^2-10|k|) dt hat{u^{n+1}}_k = hat{u^n}_k + dt hat{g^n}_k 
% 
% hat{u^{n+1}}_k = (hat{u^n}_k + dt hat{g^n}_k)/(1 + ( k^2 - 10 |k|) dt) 
%
U_now = fft(u(:,1))';
for j=1:M
    G = convolve(U_now);
    % the zero mode:
    U_new(1) = U_now(1) + .01*dt*G(1);
    for k = 1:floor(N/2)-1
        U_new(k+1) = (U_now(k+1) + .01*dt*G(k+1))/(1 + (k^2-10*abs(k))*dt);
        U_new(N-k+1) = (U_now(N-k+1) + .01*dt*G(N-k+1))/(1 + (k^2-10*abs(k))*dt);
    end
    % update so I'm ready for another time step.
    U_now = U_new;
    % reverse transform to get u.  Note that in a real computation I might
    % take many timesteps before "printing out" a solution.  And I need to
    % transform back to real space only when I want to print out a
    % solution.
    u(:,j+1) = real(ifft(U_new));
    % note: for a linear system, I wouldn't have to take the real part. If
    % I've coded everything correctly then I'll have complex conjugates
    % where I need them and the ifft will return a real-valued function.
    % But when I have a nonlinearity things get messed up and I have to
    % take the real part of the ifft
    [kk,amp(:,j+1)] = find_spec(u(:,j+1));
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% this takes the fourier transform of u and returns the fourier transform
% of u^3 u_x.  
%
function V = convolve(u)

N = length(u);

% I want to create u^3 u_x.  So I create
% something which will be u_x when IFFTed.
v(1) = 0;
for k=1:floor(N/2)-1
    v(k+1) = i*k*u(k+1);
    v(N-k+1) = -i*k*u(N-k+1);
end
% go into real space
v = ifft(v);
u = ifft(u);
% define u^3 u_x
v = (u.^3).*v;

% go back into frequency space
V = fft(v);
