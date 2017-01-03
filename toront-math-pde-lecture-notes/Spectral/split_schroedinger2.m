% [u,x,t,kk,amp,M,H] = split_schroedinger2(u0,K,t_0,t_f,dt,NPrint,N)
%
% this solves the periodic Schroedinger equation 
%
%    u_t = i u_xx + i K |u|^2 u      on [0,2*pi]
%
% K = +1 is the focussing case, K = -1 is defocussing.
%
% The boundary conditions are periodic boundary conditions and the
% spatial derivatives are spectral.  
% Srang splitting is used for the time stepping.  t_0 is the 
% initial time, t_f is the
% final time, dt is the timestep size, N is the number of mesh-points, and 
% NPrint is the number of printouts.  It also returns the power spectrum of the
% solution, where kk is the collection of wave numbers and amp = log(f_k^2)
% is the magnitude of the kth Fourier mode.
%
% Also, it returns M which is the mass \int |u|^2
% and H which is the hamiltonian \int |u_x|^2 - 1/2 |u|^4
%
function [u,x,t,kk,amp,M,H] = split_schroedinger2(u0,K,t_0,t_f,dt,NPrint,N)

% define the mesh in space
dx = 2*pi/N;
x = 0:dx:2*pi-dx;
x = x';
% the wave-numbers...
kk = (-N/2+1):1:(N/2-1);

% total number of steps to be taken:
Ntot = (t_f-t_0)/dt;
% test to see if Ntot is an integer, to see if your choice of dt
% is compatible with your choice of t_0 and t_f
if abs(Ntot - round(Ntot)) > 100000*eps
  disp(' ')
  disp('dt is not consistent with your choice of t_0 and t_f.  Choose another dt!')
  disp(' ')
%  break
end
Ntot = round(Ntot);
% number of steps to be taken before a (screen) printout
Nrun = Ntot/NPrint;
% test to see if Nrun is an integer to see if your choice of dt is
% consistent with NPrint
if abs(Nrun - round(Nrun)) > 1000*eps
  disp(' ')
  disp('NPrint is not consistent with dt.  Choose another NPrint!')
  disp(' ')
%  break
end
Nrun = round(Nrun);
% define the vector t that is to be printed out...
t = t_0:Nrun*dt:t_f;

% choose initial data
% u_now = cos(x) + 3*i*sin(2*x) - 8*exp(i*3*x);
% u_now = exp(exp(i*x/2).^2);

u_now = u0(1:N);

% We're computing solutions of
%
%    u_t = i u_xx + i |u|^2 u
%
% the first step is to take one time-step of size dt
% to compute the solution of u_t = i u_xx
% To do this, take the FFT of u, resulting in 
%
%        u_k exp(i k x)
%
% We know how to solve u_t = i u_xx at the spectral level,
% resulting in 
%
%       u_k exp(-i |k|^2 dt) exp(i k x)
%
% Taking the IFFT, we have a function v
%
%   v = IFFT( exp(-i |k|^2 dt) FFT(u) )
% 
% We now use v as initial data to take one timestep of size
% dt to sove the problem u_t = i |u|^2 u
% this is just an ODE and at each meshhpoint it has the solution
%
%       exp(i |v(x)|^2 dt) v(x)
%
% We take this function as our solution u at time t + dt to
% the NLS.

% save first printout:
u(:,1) = u_now;
[M(1),H(1)] = conserved(u_now,K,dx,N);
% save the power spectrum 
v = fft(u_now);
amp(N/2:N-1,1) = log10( abs(v(1:N/2))/N + 1.e-16);
amp(1:N/2-1,1) = log10( abs(v(N/2+2:N))/N + 1.e-16);
for jj = 1:NPrint
  for j=1:Nrun
    u_now = tstep(u_now,K,dt,N);
  end
  % save printout
  u(:,jj+1) = u_now;
  [M(jj+1),H(jj+1)] = conserved(u_now,K,dx,N);
  v = fft(u_now);
  amp(N/2:N-1,jj+1) = log10( abs(v(1:N/2))/N + 1.e-16);
  amp(1:N/2-1,jj+1) = log10( abs(v(N/2+2:N))/N + 1.e-16);
end
x(N+1) = 2*pi;
u(N+1,:) = u(1,:);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function u_now = tstep(u_now,K,dt,N);

% take one time-step of size dt/2 of the equation u_t = i u_xx
v = fft(u_now);
ii = 1:N/2;
k = (ii-1);
v(ii) = exp(-i*k.^2*dt/2).*v(ii);    
ii = N/2+2:N;
k = (ii-N-1);
v(ii) = exp(-i*k.^2*dt/2).*v(ii);    
v = ifft(v);

% now take one time-step of size dt of u_t = i K |u|^2 u,
% using v as our initial data
v = exp(i*K*abs(v).^2*dt).*v;

% first take one time-step of size dt/2 of the equation u_t = i u_xx
v = fft(v);
ii = 1:N/2;
k = (ii-1);
v(ii) = exp(-i*k.^2*dt/2).*v(ii);    
ii = N/2+2:N;
k = (ii-N-1);
v(ii) = exp(-i*k.^2*dt/2).*v(ii);    
u_now = ifft(v);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [M,H] = conserved(u,K,dx,N);
%
% use the Trapezoidal rule to compute the mass M and hamiltonian H
%             M = \int |u|^2
%             H = \int |u_x|^2 - K/2 |u|^4
%

arg = abs(u(1:N)).^2 ;
M = dx*sum(arg);

v = fft(u);
ii = 1:N/2;
k = (ii-1);
v(ii) = (i*k).*v(ii);
ii = N/2+2:N;
k = (ii-N-1);
v(ii) = (i*k).*v(ii);
% take the inverse fourier transform.  This gives u_x.
v = ifft(v);

arg = abs(v(1:N)).^2 - K/2*abs(u(1:N)).^4;
H = dx*sum(arg);

