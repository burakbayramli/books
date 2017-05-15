% Reassigned STFT
% Example of siren
clear all;
[y,fs]=wavread('srn.wav'); %read wav file
Ny=length(y);

% Reassignment-------------------------------------------

%original spectrogram
nft=256; %FFT length
Nw=256; %window length
m=Nw/2;
W=hamming(Nw);
SY=specgram(y,nft,fs,W,m);
sqm=abs(SY).^2;
nzix=find(sqm>0); %non-zero elements

%build reassignment windows
m=Nw/2;
%frequency ramp
framp=[(0:m-1),(-m:-1)]'+0.5;
framp=framp/Nw;
Wx=-imag(ifft(framp.*fft(W)));
Wdt=Wx*fs;
%time ramp
tramp=(-m:m-1)'+0.5;
Wx=tramp.*W;
Wt=Wx/fs;

%compute auxiliary spectrograms
SYdt=specgram(y,nft,fs,Wdt,m);
SYt=specgram(y,nft,fs,Wt,m);

%compute freq. corrections
[nr,nc]=size(SY);
fcorrect=zeros(nr,nc);
fcorrect(nzix)=-imag(SYdt(nzix).*conj(SY(nzix)))./sqm(nzix);
%analysis bin freqs (Hz)
Fb=((0:nr-1)'*fs/nft)*ones(1,nc); 
rF=Fb+fcorrect; %reassigned freqs

%compute time corrections
tcorrect=zeros(nr,nc);
tcorrect(nzix)=real(SYt(nzix).*conj(SY(nzix)))./sqm(nzix);
%analysis frame times (sec)
framets=(((Nw-1)/2)+(ones(nr,1)*(0:nc-1))*(Nw-m))/fs;
rT=framets+tcorrect; %reassigned times (sec)

%image plot preparation ------------------------------------

%crop & threshold
fmax=0.5*fs; fmin=0;
tmax=Ny/fs; tmin=0;
thr=-50; %threshold in dB (edit!)--------
Smax=max(abs(SY(:)));
Mx=20*log10(abs(SY)/Smax);
inzone=find(rF<fmax & rF>fmin & rT<tmax & rT>tmin);
ax=find(Mx>thr);
vdx=intersect(inzone,ax);
cSY=SY(vdx); %it is a vector
cF=rF(vdx);
cT=rT(vdx);

%create image
nh=max(500,size(SY,2)*2);
nv=max(400,size(SY,1)*2);

Tmax=max(cT); Tmin=min(cT);
dt=(Tmax-Tmin)/(nh-2);
nmax=ceil(Tmax/dt); nmin=floor(Tmin/dt);
Tn=Tmin+(dt*(0:nmax-nmin));

Fmax=max(cF); Fmin=min(cF);
df=(Fmax-Fmin)/(nv-2);
kmax=ceil(Fmax/df); kmin=floor(Fmin/df);
Fk=Fmin+(df*(0:kmax-kmin));

% Z
Z=zeros(nv,nh);
for nn=1:length(cSY),
    n=1-nmin+(cT(nn)/dt);
    k=1-kmin+(cF(nn)/df);
    alpha=n-floor(n); beta= k-floor(k);
    kf=floor(k); kc=ceil(k); nf=floor(n); nc=ceil(n);
    Z(kf,nf)=Z(kf,nf)+((1-alpha)*(1-beta)*cSY(nn));
    Z(kc,nf)=Z(kc,nf)+((1-alpha)*(beta)*cSY(nn));
    Z(kf,nc)=Z(kf,nc)+((alpha)*(1-beta)*cSY(nn));
    Z(kc,nc)=Z(kc,nc)+((alpha)*(beta)*cSY(nn));
end;

% applying the threshold
Zmin=10^(0.05*thr);
Zbak=10^(0.05*(thr-10)); %background
aux=find(abs(Z)<=Zmin);
Z(aux)=Zbak; %background includes values below threshold

% display ----------------------------------------------
figure(1)
specgram(y,nft,fs);
title('spectrogram of siren, before reassignment');

figure(2)
imagesc(Tn,Fk,20*log10(abs(Z)));
axis([min(Tn),max(Tn),min(Fk),max(Fk)]); axis xy;
colormap(1-gray); %gray scale
title('Reassigned spectrogram of the siren signal');
XLABEL('Time'); ylabel('Frequency');
