
%   This short program calculates the fourier transform of a function f(x)
%   according to the formula

%        fhat(k) = integral f(x) exp(-i*x*k) dx.

%   User must input the value a. f is then sampled over the interval
%   -a < x < a at N points. Currently N = 8192. The transform is
%   computed using the fast fourier transform of matlab. The result
%   (after rescaling) is computed on the interval -K < k < K where
%    K = pi*N/(2a). The output vector is fhat.

%   The function f is provided in the supporting mfile f.m.



a = input('Enter the value of a   ')

N = input('Enter the value of N (should be a of 2)    ')

delx = 2*a/N;
x = -a : delx :a-delx;
x(N/2+1) = .000001;

dels = pi/a;

s = -(N/2)*dels:dels:((N-1)/2)*dels;
u = f(x);
v = delx.*fft(u);

F = [v((N/2) +1: N), v(1:N/2)];
fhat  = exp(i*a*s).*F;

plot(s,fhat)





