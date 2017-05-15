%Fractional Fourier transform
%using decomposition
%Study for a set of exponents

% the signal to be transformed----------------------------
%rectangular signal
y=[zeros(70,1);ones(301,1);zeros(70,1)];
Ny=length(y); %odd length

ry=zeros(5,Ny); %room for outputs

for nn=1:5,
%choose parameter a (fractional power) 0.5<a<1.5

if nn==1, a=0.55; end;
if nn==2, a=0.7; end;
if nn==3, a=0.8; end;
if nn==4, a=0.9; end;
if nn==5, a=0.99; end;

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

%result recording
aux=real(yp);
ry(nn,:)=aux(:)';

end;

% display-------------------------------------
figure(1)
subplot(5,1,1)
plot(y,'k');
axis([0 Ny -0.1 1.1]);
title('a rectangular signal');
subplot(5,1,2)
plot(ry(1,:),'k'); axis([0 Ny -3 3]);
title('Fractional Fourier transform (a=0.55)');
subplot(5,1,3)
plot(ry(2,:),'k'); axis([0 Ny -3 3]);
title('a=0.7');
subplot(5,1,4)
plot(ry(3,:),'k');axis([0 Ny -3 3]);
title('a=0.8');
subplot(5,1,5)
plot(ry(4,:),'k');axis([0 Ny -3 3]);
title('a=0.9');

figure(2)
plot(ry(5,:),'k');
axis([180 Ny-180 -4 9]);
title('Fractional Fourier transform (a=0.99)');
