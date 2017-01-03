%5.3  inversezcode.m

N = 32; R = 2; k = [0:N-1]; theta = 2*pi*k/N; % N points on circle |z|=R
U = @(z) (1./z)./(1-1./z).^2; % Inverse transform of U(z) is u_n=n
% Also try U=@(z) (1./z).*(1+1./z)./(1-1./z).^3 % U inverts to u_n=n^2
z = R*exp(i*theta); u = (R.^k).*ifft(U(z)); % Find u by sum around |z|=R
