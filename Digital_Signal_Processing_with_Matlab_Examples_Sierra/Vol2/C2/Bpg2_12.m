% Computing a scaling function using FFT and MAE
% Display of final result

%Daubechies 4, coeffs.
hden=4*sqrt(2); %coeff. denominator
hsq=sqrt(3); %factor
h=[(1+hsq)/hden, (3+hsq)/hden, (3-hsq)/hden, (1-hsq)/hden]; %Daubechies 4
hN=(h*2)/sum(h); %normalization

Ns=2^12; %number of samples

Ffi=fft(hN,Ns); %Initial Fourier transform
aux=Ffi;

for nn=1:8,
   Ffi=[Ffi(1:2:Ns),Ffi(1:2:Ns)];
   aux=(aux.*Ffi)/sqrt(2);   
end;

fi=real(ifft(aux));

L=6*(2^8); %the supported part
t=((0:L-1)*3)/L; 
ct=2^4; fi=ct*fi; %scaling
plot(t,fi(1:L),'k')
title('Daubechies 4 scaling function');