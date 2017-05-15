% Wiener FIR filter coefficients
% sine signal + noise
fx=1; %signal frequency in Hz
wx=2*pi*fx; %signal frequency in rad/s

fs=60; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(50-tiv); %time intervals set

x=sin(wx*t); %signal data set
Nx=length(x);
v=randn(1,Nx); %normal noise
y=x+v; %sine+noise

Nh=50; %number of FIR coeffs

%Wiener computations
syy=xcorr(y); %symmetrical auto-correlation sequence 
Ry=toeplitz(syy(Nx:Nx+Nh-1)); %Ry matrix
sxy=xcorr(x,y); %symmetrical cross correlation
rxy=sxy(Nx:Nx+Nh-1); %rxy vector
wh=Ry\rxy'; %Wiener FIR coeffs.

%append for FIR symmetry
rwh=wh';
lwh=fliplr(rwh); 
sh=0.5*[lwh rwh]; %symmetrical FIR

fly=filter(sh,1,y); %apply the Wiener filter

%display---------------------------

figure(1)
plot(wh,'k'); %computed FIR coefficients (right-side)
title('FIR coefficients (right-side)');

figure(2)
plot(sh,'k'); %Symmetrical FIR coefficients
title('Symmetrical FIR coefficients');

figure(3)
plot(t,fly,'k'); %filtered signal
axis([0 4 -3.5 3.5]);
xlabel('seconds'); title('filtered signal');
