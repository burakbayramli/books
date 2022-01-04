%                 Program kdv
%
%   This program uses a pseudo spectral method  with preconditioning
%   to integrate the initial value problem (with periodic initial data)
%   of the KdV  equation 
%                u_t + uu_x  +gamma*u_xxx  = 0
%
%   The program uses the MATLAB ode solver ode45 and supporting mfiles
%   wwprime.m. User must provide the initial data with period 2*pi in
%   the mfile f.m
%
%   CAUTION: The calls for ode45 in MATLAB4.2 and MATLAB5.0 are 
%   slightly different. Use the appropriate line in the code where
%   ode45 is called.
%   
%   User enters the number of equations to be solved ( same as number of
%   sample points of the initial data) and the time T at which the
%   the solution is to be viewed. Solution at time T and initial data
%   are plotted together.





clear all
global N kvec symbol

gamma = input('enter the value of gamma  ')
N = input('enter the value of N   ')
T = input('enter the time T to view the solution   ')

M = N/2;
kvec = fftshift([-M:M-1]);
kvec = kvec';
% the symbol multiplier is a column vector.
symbol = gamma *(i*kvec).^3;

delx = 2*pi/N;
x = 0:delx:2*pi-delx;

u0 = f(x); % complex row vector, u0.' is a complex column vector, without
           %taking complex conjugates.
w0 = fft(u0.')/N; % complex N column vector.

% For ode45 in MATLAB4.2, we need a real 2N column vector.
ww0(1:N,1) = real(w0) ; ww0(N+1:2*N,1) = imag(w0);

% ode45 uses column vector for input, but the output is in rows.
 [t,ww] = ode45('wwprime', 0, T, ww0); % command for matlab4.2
%[t,ww] = ode45('wwprime', [0,T], ww0); % command for matlab5.0

nsteps = size(t,1);
wwout = ww(nsteps,:); %output is a real 2N row vector.
wout = wwout(1:N) +i*wwout(N+1:2*N);
uhatout = exp(-T*symbol)'.*wout;
uT = ifft(uhatout)*N;

xplot = [x,2*pi];
u0plot = [u0, u0(1)];
uTplot = [uT,uT(1)];

plot(xplot, u0plot,xplot,uTplot)
axis([0 2*pi, -2 2])
