%5.1  taylorcoeffcode.m

f = @(x) exp(x);            % f(x) = e^x has f^(n)(0) = 1 and a_n = 1/n!
z = exp(2*i*pi*(0:N-1)'/N); % N equally spaced points on |z| = 1
a = fft(f(z)/N);            % FFT gives a_0 to a_{N-1} with high accuracy
a = real(a);                % Those coefficients are real by symmetry
disp([a 1./gamma(1:N)'])    % Display computed and exact a_n=1/n!
