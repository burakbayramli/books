%3.5  sinetransform.m

v=[0; u; zeros(N+1,1)]; z = fft(v); % Fourier transform of size M
Su = -imag(z(2:N+1));               % Sine transform of size N
