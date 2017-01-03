% [u,x,t,kk,amp] = burgers(t_0,t_f,M,N)
%
% solves the periodic Burgers equation without viscosity:
%                u_t + u u_x = u_t + (u^2/2)_x = 0 
% with initial data u_0 = cos(x) with periodic boundary conditions using 
% spectral derivatives in
% space and explicit time-stepping.  t_0 is the initial time, t_f is the
% final time, N is the number of mesh-points, and M is the number of
% time steps.  err is the error. it also returns the power spectrum of the
% solution, where kk is the collection of wave numbers and amp = log(f_k^2)
% is the magnitude of the kth Fourier mode.
%
% N SHOULD BE A POWER OF 2!  
%
% (It will work no matter what, but will go much faster if N is a power
% of 2.)
%
% Note: for burgers equation with initial data cos(x), we know that the 
% solution blows up at time: t = 1

function [u,x,t,kk,amp] = burgers(t_0,t_f,M,N)

% define the mesh in space
dx = 2*pi/N;
x = 0:dx:2*pi-dx;
x = x';
kk = 0:1:(N/2-1);

% define the mesh in time
dt = (t_f-t_0)/M;
t = t_0:dt:t_f;

u(:,1) = sin(x);
v = fft(u(:,1));
for k=0:(N/2-1)
  if k == 0 
    amp(k+1,1) = log10(sqrt(v(1+k)^2)/N + 10e-16);
  else
    amp(k+1,1) = log10(sqrt(real(v(1+k)*v(N-k+1)))/N + 1e-16);    
  end
end

% first I take an explicit Euler Step:
% u_t + (u^2/2)_x = 0.  Hence u_t = - (u^2/2)_x.  On the level of
% fourier coefficients: d/dt u(k) = - i k temp(k) where temp = u_old^2/2
%
% Method: Take the fourier transform of temp and of u_old.  At each
% mode, multiply by temp's fft by -dt*i*k where k is the mode-number.  
% write u_new = u_old - dt*i*k*temp.  Take the inverse fft.
%
% I HAVE TO BE CAREFUL NOT TO USE "i" AS A LOOP INDEX ANYWHERE SINCE
% MATLAB WILL THEN FORGET THAT "i" IS THE SQUARE ROOT OF -1!
temp = u(:,1).^2/2;
temp = fft(temp);
v = fft(u(:,1));
for k = 1:(N/2-1)
% k is the wave-number.  Note that the zero mode v(1) is untouched since
% it is the mean.
  v(1+k) = v(1+k) - dt*i*k*temp(1+k);
  v(N-k+1) = v(N-k+1) - dt*i*(-k)*temp(N-k+1);
end
j=1;
for k=0:(N/2-1)
  if k == 0 
    amp(k+1,j+1) = log10(v(1+k)/N + 10e-16);
  else
    amp(k+1,j+1) = log10(sqrt(real(v(1+k)*v(N-k+1)))/N + 10e-16);    
  end
end
% take the inverse transform.  I've cut off the imaginary parts since
% they're going to be at the level of round-off anyway.  (the true
% solution started out real-valued and will remain real.  If I wanted to
% be super-cautious, I'd keep the imaginary parts of the computed
% solution around to make sure they aren't growing on me.)
u(:,2) = real(ifft(v)); 


% now I switch to leap-frog time-stepping, which is linearly stable
% for hyperbolic equations 
for j=2:M
  temp = u(:,j).^2/2;
  temp = fft(temp);
  v = fft(u(:,j-1));
  for k = 1:(N/2-1)
% k is the wave-number.  Note that the zero mode v(1) is untouched since
% it is the mean.
    v(1+k) = v(1+k) - 2*dt*i*k*temp(1+k);
    v(N-k+1) = v(N-k+1) - 2*dt*i*(-k)*temp(N-k+1);
  end
  for k=0:(N/2-1)
    if k == 0 
      amp(k+1,j+1) = log10(v(1+k)/N + 10e-16);
    else
      amp(k+1,j+1) = log10(sqrt(real(v(1+k)*v(N-k+1)))/N + 10e-16);    
    end
  end
  u(:,j+1) = real(ifft(v)); 
end

