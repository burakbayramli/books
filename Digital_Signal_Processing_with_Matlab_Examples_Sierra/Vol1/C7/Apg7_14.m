%Fractional Fourier transform
%using decomposition

%choose parameter a (fractional power) 0<a<1.5

a=0.55; %for instance

% the signal to be transformed----------------------------
%cosine signal
t=0:0.015:2*pi;
y=cos(t);
Ny=length(y); %odd length
yin=y;

%changes for a<0.5
if (a<0.5),     
shft = rem((0:Ny-1)+fix(Ny/2),Ny)+1;
sqN = sqrt(Ny);
a=a+1; y(shft)=ifft(y(shft))*sqN;
end;

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

% display-------------------------------------
figure(1)
subplot(2,1,1)
plot(yin,'k');
%axis([0 2*pi -1.1 1.1]);
title('a cosine signal');
subplot(2,1,2)
plot(real(yp),'k');
%axis([0 2*pi -2 2]);
title('Fractional Fourier transform (a=0.55)');

