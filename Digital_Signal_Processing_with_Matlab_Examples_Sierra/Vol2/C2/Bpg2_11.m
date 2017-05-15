% Computing a scaling function using FFT and MAE
% Display of first iterations

%Daubechies 4, coeffs.
hden=4*sqrt(2); %coeff. denominator
hsq=sqrt(3); %factor
h=[(1+hsq)/hden, (3+hsq)/hden, (3-hsq)/hden, (1-hsq)/hden]; %Daubechies 4
hN=(h*2)/sum(h); %normalization

Ns=2^12; %number of samples

Ffi=fft(hN,Ns); %Initial Fourier transform
aux=Ffi;

for nn=1:6,
   %display
   subplot(2,3,nn); 
   plot(abs(aux(1:(Ns/2))),'k'); axis([0 Ns/2 0 2*nn]);
   %iteration
   Ffi=[Ffi(1:2:Ns),Ffi(1:2:Ns)];
   aux=(aux.*Ffi)/sqrt(2);   
end;

