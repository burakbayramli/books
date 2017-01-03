%do_RDFT
clear, clf
N=128; k=[0:N-1];
x=zeros(1,N); %initialize the data block
Xr=zeros(1,N); % and its DFT 
for m=0:N
   xN=rand; %new data
   Xr=(Xr+xN-x(1)).*???????????????? %RDFT formula (P3.20-5)
   x=[x(2:N) xN];
end
dif=norm(Xr-fft(x)) %difference between RDFT and FFT 
