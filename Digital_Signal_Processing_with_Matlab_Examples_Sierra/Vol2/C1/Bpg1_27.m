% Auditive check of PR
% 2-channel filter bank, and music
[u,fs]=wavread('B_allyou.wav'); %read wav file
L=length(u);

%biorthogonal filter bank
h0=[1 2 1]; %the filter H0
h0=(1/4)*h0; %scaling
fx=conv([1 2 1],[-1 4 -1]); 
fx=(1/8)*fx; %scaling
f0=2*fx; %the filter F0
for n=1:5, h1(n)=((-1)^(n-1))*fx(n); end; % the filter H1
for n=1:3, f1(n)=-2*((-1)^(n-1))*h0(n); end; % the filter F1

%filtering and downsampling
v0=filter(h0,1,u); dv0=decimate(v0,2);
v1=filter(h1,1,u); dv1=decimate(v1,2);

%upsamplng and filtering
uv0=zeros(L,1); uv0(1:2:L)=dv0;
uv1=zeros(L,1); uv1(1:2:L)=dv1;
y0=filter(f0,1,uv0);
y1=filter(f1,1,uv1);

%final adding
yr=y0+y1;

sound(v0,fs);
xcr=xcorr(u,yr); %cross-correlation of output and input

%display
figure(1)
plot(xcr,'k');
axis([0 2*L -1e4 2.5e4]);
title('cross-correlation of input and output');



