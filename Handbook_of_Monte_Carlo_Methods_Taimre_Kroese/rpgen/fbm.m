%fbm.m
N=2^15; 
sig = 1;
t= 0:N;
H = 0.9; %Hurst parameter
sigma(1) = sig;
for k=1:N
   sigma(k+1) = 0.5*sig*((k+1)^(2*H) - 2*k^(2*H) + (k-1)^(2*H));
end
c=[sigma sigma((end-1):-1:2)]';
lambda=fft(c); %eigenvalues
eta=sqrt(lambda./(2*N));
Z=randn(2*N,1)+sqrt(-1).*randn(2*N,1); %complex normal vectors
Zeta= Z.*eta;
X2n=fft(Zeta);
A=X2n(1:(N+1));
X=real(A);
c = 1/N;
X1 = c^H*cumsum(X);
plot(t*c,X1);
