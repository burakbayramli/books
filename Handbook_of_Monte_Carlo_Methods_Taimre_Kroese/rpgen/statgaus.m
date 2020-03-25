%statgaus.m
N=10^4; a=0; b=5;
t=linspace(a,b,N+1); sigma=exp(-(t-t(1)));
c=[sigma sigma((end-1):-1:2)]';
lambda=fft(c); %eigenvalues
eta=sqrt(lambda./(2*N));
Z=randn(2*N,1)+sqrt(-1).*randn(2*N,1); %complex normal vectors
Zeta= Z.*eta;
X2n=fft(Zeta);
A=X2n(1:(N+1));
X=real(A);
plot(t,X)
