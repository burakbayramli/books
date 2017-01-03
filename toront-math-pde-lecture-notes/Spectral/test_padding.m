
N = 30;
dx = 2*pi/N;
x = 0:dx:2*pi-dx;
u = exp(cos(x));
% here is the true value of u u_x
v = u.*(-sin(x).*exp(cos(x)));

% compute u u_x with padding
U = fft(u);
V1 = convolve(U);
v1 = real(ifft(V1));

% compute u u_x with no padding
V2 = convolve_nopadding(U);
v2 = real(ifft(V2));

max(abs(v-v1))
max(abs(v-v2))
