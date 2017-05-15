%Fractional Fourier transform
%using decomposition

% the signal to be transformed----------------------------
%rectangular signal
y=[zeros(70,1);ones(301,1);zeros(70,1)];
Ny=length(y); %odd length

ry=zeros(4,Ny,Ny); %room for outputs

for nn=1:4,
%choose parameter a (fractional power) 0.5<a<1.5

if nn==1, a=0.55; end;
if nn==2, a=0.7; end;
if nn==3, a=0.8; end;
if nn==4, a=0.9; end;

alpha=a*pi/2;

%sinc interpolation for doubling signal data
zy=zeros(2*Ny-1,1);
zy(1:2:2*Ny-1)=y;
aux1=zy(1:2*Ny-1); aux2=sinc([-(2*Ny-3):(2*Ny-3)]'/2);
m=length([aux1(:);aux2(:)])-1;
P=2^nextpow2(m);
yitp=ifft(fft(aux1,P).*fft(aux2,P)); %convolution using fft
yitp=yitp(1:m);
yitp=yitp(2*Ny-2:end-2*Ny+3); %interpolated signal

%sandwich
zz=zeros(Ny-1,1);
ys=[zz; yitp; zz];

% the fractional transform-------------------------------

%chirp premultiplication
htan=tan(alpha/2);
aex=(pi/Ny)*(htan/4)*((-2*Ny+2:2*Ny-2)'.^2);
chr=exp(-j*aex);
yc=chr.*ys; %premultiplied signal

%chirp convolution 
sa=sin(alpha);
cc=pi/Ny/sa/4;
aux1=exp(j*cc*(-(4*Ny-4):4*Ny-4)'.^2);
m=length([aux1(:);yc(:)])-1;
P=2^nextpow2(m);
ym=ifft(fft(aux1,P).*fft(yc,P)); %convolution using fft
ym=ym(1:m);
ym=ym(4*Ny-3:8*Ny-7)*sqrt(cc/pi); %convolved signal

%chirp post multiplication
yq=chr.*ym;

%normalization
yp=exp(-j*(1-a)*pi/4)*yq(Ny:2:end-Ny+1);

%Wigner analysis 
yh=hilbert(yp);
zerx=zeros(Ny,1); aux=zerx;
lm=(Ny-1)/2;
zyz=[zerx; yh; zerx]; %sandwich zeros-signal-zeros
WD=zeros(Ny,Ny); %space for the Wigner distribution, a matrix
mtau=0:lm; %vector(used for indexes)
for nt=1:Ny, 
    tpos=Ny+nt+mtau; %a vector
    tneg=Ny+nt-mtau; %a vector
    aux(1:lm+1)=(zyz(tpos).*conj(zyz(tneg)));
    aux(1)=0.5*aux(1); %will be added 2 times
    fo=fft(aux,Ny)/Ny;
    WD(:,nt)=2*real(fo); %a column (harmonics at time nt)
end 

% using a threshold for interference attenuation
msx=(abs(WD)>0.14); %matrix of 0 or 1 entries
fWD=WD.*msx; %select SAF entries over threshold

ry(nn,:,:)=fWD; %for figure 1

end;

% display-------------------------------------
aux=zeros(Ny,Ny); k=10; hh=1:Ny; vv=1:Ny-150;
figure(1)
subplot(2,2,1)
aux(:,:)=ry(1,:,:);
imagesc(log10(k+abs(aux(vv,hh)))); axis xy;
title('a=0.55');
subplot(2,2,2)
aux(:,:)=ry(2,:,:);
imagesc(log10(k+abs(aux(vv,hh)))); axis xy;
title('a=0.7');
subplot(2,2,3)
aux(:,:)=ry(3,:,:);
imagesc(log10(k+abs(aux(vv,hh)))); axis xy;
title('a=0.8');
subplot(2,2,4)
aux(:,:)=ry(4,:,:);
imagesc(log10(k+abs(aux(vv,hh)))); axis xy;
title('a=0.9');


