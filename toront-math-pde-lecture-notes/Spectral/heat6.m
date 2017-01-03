% [u,err,x,t,kk,amp] = heat6(t_0,t_f,M,N,u0)
%
% this solves the periodic heat equation u_t = u_xx with initial data u_0 
% with periodic boundary conditions using spectral derivatives in
% space and Crank-Nicolson time-stepping.  t_0 is the initial time, t_f is the
% final time, N is the number of mesh-points, and M is the number of
% time steps.  err is the error. it also returns the power spectrum of the
% solution, where kk is the collection of wave numbers and amp = log(f_k^2)
% is the magnitude of the kth Fourier mode.
%
% N SHOULD BE A POWER OF 2!  
%
% (It will work no matter what, but will go much faster if N is a power
% of 2.)

function [u,err,x,t,kk,amp] = heat6(t_0,t_f,M,N,u0)

% define the mesh in space
dx = 2*pi/N;
x = 0:dx:2*pi-dx;
x = x';
kk = 0:1:(N/2-1);

% define the mesh in time
dt = (t_f-t_0)/M;
t = t_0:dt:t_f;

D = 1;
% define the ratio r
r = D*dt/dx^2 ;

u(1:N,1) = u0;
err(:,1) = zeros(size(x));
[kk,amp(:,1)] = find_spec(u(:,1));

% take the fourier transform of u_old.  At each mode, multiply by 1 -
% dt*k^2 where k is the mode-number.  Then take the inverse fourier
% transform

for j=1:M
  v = fft(u(:,j));
  for k = 1:floor(N/2)-1
% k is the wave-number.  Note that the zero mode v(1) is untouched since
% it is the mean.
    v(k+1) = v(k+1)*((1-.5*D*dt*k^2)/(1+.5*D*dt*k^2));
    v(N-k+1) = v(N-k+1)*(1-.5*D*dt*(-k)^2)/(1+.5*D*dt*(-k)^2);
  end
  v(floor(N/2)+1) = 0;
% take the inverse transform.  I've cut off the imaginary parts since
% they're going to be at the level of round-off anyway.  (the true
% solution started out real-valued and will remain real.  If I wanted to
% be super-cautious, I'd keep the imaginary parts of the computed
% solution around to make sure they aren't growing on me.)
  u(:,j+1) = real(ifft(v)); 
  [kk,amp(:,j+1)] = find_spec(u(:,j+1));
  
  % the exact solution:
  v = fft(u(:,1));
  for k = 1:floor(N/2)-1
% k is the wave-number.  Note that the zero mode v(1) is untouched since
% it is the mean.
    v(k+1) = exp(-D*k^2*(t(j+1)-t(1)))*v(k+1);
    v(N-k+1) = exp(-D*(-k)^2*(t(j+1)-t(1)))*v(N-k+1);
  end
  % zero out the isolated highest mode...
  v(floor(N/2)+1) = 0;
  err(:,j+1) = u(:,j+1) - real(ifft(v));
end
% impose periodicity
u(N+1,:) = u(1,:);
x(N+1) = 2*pi;

