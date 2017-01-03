% [u,x,t,kk,amp] = burgers2(u0,D,t_0,t_f,M,N)
%
% solves the periodic Burgers equation without viscosity:
%                u_t + u u_x = D u_xx
% with initial data u_0 with periodic boundary conditions using 
% spectral derivatives in
% space and explicit time-stepping.   The nonlinearity is done explicitly.
% the linear term is propagated forward "exactly" as in an integrating 
% factor method.
%
% t_0 is the initial time, t_f is the
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
% Note: for burgers equation with initial data cos(x), we know that the 
% solution blows up at time: t = 1
%
%

function [u,x,t,kk,amp] = burgers2(u0,D,t_0,t_f,M,N)

% define the mesh in space
L = 2*pi;
dx = L/N;
x = 0:dx:L-dx;
x = x';
kk = 0:1:(N/2-1);

% define the mesh in time
dt = (t_f-t_0)/M;
t = t_0:dt:t_f;


u(:,1) = u0;
[kk,amp(:,1)] = find_spec(u0);

% At time t_n, I use convolve.m to compute the nonlinearity u^n u^n_x.  It
% gives me the fourier transform of u^n u^n_x.  Let's call that g^n.  And
% so in Fourier space I have the ODE.  I use an integrating factor method
% to timestep.
%
%     d/dt hat{u^n}_k + hat{g^n}_k = - D k^2 (2*pi/L)^2 hat{u^n}_k
%     d/dt hat{u^n}_k +  D k^2 (2*pi/L)^2 hat{u^n}_k = - hat{g^n}_k
%     d/dt ( exp( D k^2 (2*pi/L)^2 t ) hat{u^n}_k ) = - hat{g^n}_k
%     exp( D k^2 (2*pi/L)^2 dt ) hat{u^{n+1}}_k = hat{u^n}_k - dt hat{g^n}_k
%     hat{u^{n+1}}_k = exp{- D k^2 (2*pi/L)^2 dt}( hat{u^n}_k - dt hat{g^n}_k )


U_now = fft(u(:,1));
for j=1:M
    G = convolve(U_now);
    % the zero mode:
    U_new(1) = U_now(1) - dt*G(1);
    for k = 1:floor(N/2)-1
        U_new(k+1) = exp(-D*k^2*(2*pi/L)^2*dt)*(U_now(k+1) - dt*G(k+1));
        U_new(N-k+1) = exp(-D*k^2*(2*pi/L)^2*dt)*(U_now(N-k+1) - dt*G(N-k+1));
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

