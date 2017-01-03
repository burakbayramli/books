% this takes the fourier transform of u and returns the fourier transform
% of u u_x.  This is to be used in the timestepping for burger's equation.
%
function V = convolve(u)

N = length(u);

U = zeros(1,N);
% u is a bunch of Fourier Coefficients.  I want to create U which is
% padded with zeros in the middle.
U(1:floor(N/2)) = u(1:floor(N/2));
U(N-floor(N/2)+1:N) = u(floor(N/2)+1:N);

% I want to create u u_x.  So I first copy U into a new vector and create
% something which will be u_x when IFFTed.
V(1) = 0;
for k=1:floor(N/2)-1
    V(k+1) = i*k*U(k+1);
    V(N-k+1) = -i*k*U(N-k+1);
end
V = ifft(V);
U = ifft(U);
v = U.*V;
v = fft(v);
clear V
V(1) = v(1);
for k=1:floor(N/2)-1
  V(k+1) = v(k+1);
  V(N-k+1) = v(N-k+1);
end
V(floor(N/2)+1) = 0;


