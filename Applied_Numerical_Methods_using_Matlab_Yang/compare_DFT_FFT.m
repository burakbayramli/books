%compare_DFT_FFT
clear, clf
N=2^10; n=[0:N-1];
%x=rand(1,N)-0.5;
x=cos(2*pi*200/N*n)+0.5*sin(2*pi*300/N*n);
tic
%flops(0)
for k=0:N-1
   X(k+1)=x*exp(-j*2*pi*k*n/N).'; %DFT
end
k=[0:N-1]; X1(k+1)=x*exp(-j*2*pi*n'*k/N).'; %DFT
dif1=norm(X1-X)
%k=[0:N-1];
for n=0:N-1
   xr(n+1)=X*exp(j*2*pi*k*n/N).'; %IDFT
end
n=[0:N-1]; xr1(n+1)=X1*exp(j*2*pi*k'*n/N).'; %DFT
dif2=norm(xr1-xr)
%flop_dft=flops %number of floating-point operations
toc
stem(k,abs(X),'.')
axis([0 N-1 0 600]), pause
tic
%flops(0)
X1=fft(x); %FFT
xr1=ifft(X1); %IFFT
%flop_fft=flops %number of floating-point operations
toc
hold on, stem(k,abs(X1),'r.')
%flps_ratio=flop_dft/flop_fft %compare number of floating-point operations
discrepancy=norm(X-X1)/norm(X) %Are X and X1 discrepant?