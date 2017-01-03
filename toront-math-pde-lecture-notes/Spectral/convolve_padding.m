% this takes the fourier transform of u and returns the fourier transform
% of u u_x.  This is to be used in the timestepping for burger's equation.
%
function V = convolve(u)

N = length(u);
M = 2*N;

U = zeros(1,M);
% u is a bunch of Fourier Coefficients.  I want to create U which is
% padded with zeros in the middle.
U(1:floor(N/2)) = u(1:floor(N/2));
U(M-floor(N/2)+1:M) = u(floor(N/2)+1:N);
% since I doubled the number of points, I need to double the normalization
% on the Fourier modes
U = 2*U;

% I want to create u u_x.  So I first copy U into a new vector and create
% something which will be u_x when IFFTed.
V(1) = 0;
for k=1:floor(M/2)-1
    V(k+1) = i*k*U(k+1);
    V(M-k+1) = -i*k*U(M-k+1);
end
V = ifft(V);
U = ifft(U);
v = U.*V;
v = fft(v);
% now I have v but it's too big.  so I remove the padding
clear V
V(1) = v(1);
for k=1:floor(N/2)-1
  V(k+1) = v(k+1);
  V(N-k+1) = v(M-k+1);
end
V(floor(N/2)+1) = 0;
% % since I've now halved the number of points, I need to halve the
% % normalization.  
V = V/2;


