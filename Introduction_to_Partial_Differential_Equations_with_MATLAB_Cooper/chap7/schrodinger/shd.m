

%                       Program shd
%
%   Program computes the solution of the free schrodinger equation 
%   given initial data f(x) using fast fourier transform of matlab.
%   User must provide mfile for f. 
%   f must be smooth with f  approx = 0 for x outside the
%   interval [-a, a].  In addition, the Fourier transform of f
%   should be approx zero outside the interval [-N/(4*a), N/(4*a)]
%   where N = 8192.  
%   Example: f(x) = exp(-x^2/24)*exp(2ix). Here a = 16 is a good choice.
%   User enters a ( must be a power of 2) and the time t at which solution
%   is to be viewed. 
%        Solution at time t goes into the vector u, the probability 
%   density goes into the vector density, and the envelope = 
%   sqrt(density) goes into the vector env.  Plotting is done on the
%   interval [-a, a].  
%         For comparison the solution of the heat equation with the
%   same initial data is computed, also using fast fourier transform.
%   The solution at time t is put into the vector v to be plotted 
%   on the interval [-a,a].



a = input(' Enter the value of a  ')

t = input( 'Enter the value of t   ')
N = 8192;

delx = 2*a/N;
x = -a : delx : a-delx;

K = N/(4*a);
delk = 1/(2*a);
k= -K: delk :K-delk;

g = f(x);

FF = delx.*fft(g);

F = exp(2.*pi.*i.*k.*a).*[FF((N/2)+1:N),FF(1:N/2)];



% multiplication of fhat by the factor for schrodinger
uhat = exp(-i.*t.*(2.*pi.*k).^2 ).* F;

uu = N.*delk.*ifft(uhat);

uuu = [uu((N/2)+1:N), uu(1:N/2)];

% solution of schrodinger equation at time t.
u = exp(-i.*(2*pi).*K.*x).*uuu;

%calculation of prob. density for solution of schrodinger
density = (real(u)).^2 + (imag(u)).^2;

% calculation of envelope = sqrt(density)
env = sqrt(density);



%multiplication of fhat by the factor for heat equation.
vhat = exp(-t.*(2.*pi.*k).^2).*F;

vv = N.*delk.*ifft(vhat);

vvv = [vv((N/2)+1:N), vv(1:N/2)];

% solution of heat equation at time t
v = exp(-i.*(2*pi).*K.*x).*vvv;
