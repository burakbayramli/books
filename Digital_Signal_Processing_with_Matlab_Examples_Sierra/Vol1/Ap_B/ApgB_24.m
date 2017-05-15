% Short Time Fan-Chirp transform
% (as a spectrogram)
%example with quack

clear all;

% the signal
[y,fs]=wavread('duck_quack.wav'); %read wav file

% transform parameters
nft=256; %FFT length
Ny=length(y); %signal length, even
nw=97; %window size, odd
nov=90; %overlapping
mo=nw-nov; %must be positive
win=hamming(nw); %window
win=win./sum(win);

% reference estimated  frequencies
tr=0:0.0002:0.2;
fr=40*tr;

Fc=fs*(0:nft/2)/nft; %frequency centers

% time centers:
Tc=[]; idx=1; kk=1;
while idx(end)<=Ny,
    idx=((kk-1)*mo)+(1:nw);
    if idx(end)<=Ny,
        Tc=[Tc (idx((nw-1)/2+1)-1)/fs];
    end
    kk=kk+1;    
end

% interpolated estimated frequencies

fe=interp1(tr,fr,Tc,'linear','extrap');

nTc=length(Tc);
aux=ones(nTc,1);
FT=zeros(nft/2+1,nTc);
for kk=1:nTc,
    idx=((kk-1)*(mo))+(1:nw);
    % derivative approximation:
    if kk==1 || kk==nTc,
        A=0;
    else
        A=(1/fe(kk))+(fe(kk+1)-fe(kk-1))/(Tc(kk+1)-Tc(kk-1));
    end
    A=A/fs;
    aslp(kk)=A;
    % the FCh transform for the signal frame:
      z=y(idx).*win; %signal windowed segment
      M=length(z); N=nft;
      ks=(-N/2+1:N/2)'; 
      ks=[ks(N/2:end); ks(1:N/2-1)];
      nn=-(M-1)/2:(M-1)/2;
      aux=(-2*pi/N).*ks*((1+0.5*A*nn).*nn);
      E=exp(j*aux); 
      q=ones(N,1);
      FTx=sum(q*(z'.*sqrt(abs((1+A*nn)))).*E,2);       
    FT(:,kk)=FTx(1:end/2+1);
end

% display-------------------------------------------------
figure(1)
specgram(y,nft,fs);
title('spectrogram of duck quack')

figure(2)
imagesc(Tc,Fc,20*log10(abs(FT)),[-40 -1]); axis xy;
title('Fan-Chirp transform of duck quack')
xlabel('Time'); ylabel('Frequency');

