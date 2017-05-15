% Filtered (mask) WD of Bird signal

% the signal
[yo,fs]=wavread('bird.wav'); %read wav file
tiv=1/fs;
fN=fs/2; %Nyquist freq.
% force odd length
aux=mod(length(yo),2);
if aux==0, yo=yo(1:(end-1)); end;
y=hilbert(yo);
Ny=length(y);
t=0:tiv:(Ny-1)*tiv;

%SAF------------------------------------------------------
zerx=zeros(Ny,1); %a vector
zyz=[zerx; y; zerx]; %sandwich zeros-signal-zeros
aux=zerx;
SAF=zeros(Ny, Ny); %space for the SAF, a matrix
nt=1:Ny; %vector (used for indexes)
md=(Ny-1)/2;
for mtau=-md:md, 
    tpos=Ny+nt+mtau; %a vector
    tneg=Ny+nt-mtau; %a vector
    aux=zyz(tpos).*conj(zyz(tneg));
    SAF(:,md+mtau+1)=fftshift(fft(aux,Ny)/Ny); %a column (frequencies)
end 
 
 %A simple box distribution kernel
FI=zeros(Ny,Ny);
HV=100; HH=200; %window vertical and horizontal 1/2 width
FI(md-HH:md+HH,md-HV:md+HV)=1; %box kernel

%Product of kernel and SAF
fsaf=FI.*SAF;

pks=ifftshift(fsaf); %intermediate variable
ax=((ifft(pks,[],1)));
WD=real((fft(ax,[],2))'); %Wigner from SAF distribution

%display--------------------------------------------------------
figure(1)
fiv=fN/Ny; %frequency interval
f=0:fiv:(fN-fiv); %frequency intervals set
colmap1; colormap(mapg1); %user colormap
imagesc(t,f,log10(abs(WD)),[-3 0]); axis xy;
xlabel('seconds'); ylabel('Hz'); 
title('Filtered Wigner distrib. of the Bird signal');


