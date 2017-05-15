% Noise cancellation using LMS
% sine signal + noise
fx=1; %signal frequency in Hz
wx=2*pi*fx; %signal frequency in rad/s

fs=60; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(30-tiv); %time intervals set

x=sin(wx*t); %signal data set
Nx=length(x);

u=randn(1,Nx); %reference (normal) noise
Nd=12; %delay between noises
v=zeros(1,Nx); v(1,(1+Nd):Nx)=u(1,1:(Nx-Nd)); %delayed noise
z=x+v; %sine+noise

%FIR coeff. computation
Nh=Nd; %number of FIR coefficients
wh=zeros(Nh,1); %FIR coefficients initialization
vv=zeros(Nh,1); %v vector
er=zeros(1,Nx); %for error recording
fo=zeros(1,Nx); %filter output

mu=1/(10*Nh); %learning rate

for nn=Nh:Nx-1,
   %actualization of vv
   vv=v(nn:-1:(nn-Nh+1))'; %take a segment and reverse
   %filter output
   fo(nn)=wh'*vv;
   %error
   er(nn)=z(nn)-fo(nn);
   %new FIR coeffs
   epsi=0.01; %small constant, to be edited
   aux=mu/(epsi+norm(vv));
   wh=wh+((aux*er(nn))*vv);
end;  

%append for FIR symmetry
rwh=wh';
lwh=fliplr(rwh); 
sh=0.5*[lwh rwh]; %symmetrical FIR

fly=filter(sh,1,v); %apply the Wiener filter

%display---------------------------
figure(1)
plot(t,z,'k'); %noisy signal
title('noisy signal');

figure(2)
plot(t,er,'k'); %filtered signal (the error)
title('filtered signal');

figure(3)
plot(wh,'k'); %FIR coefficients
title('FIR coefficients (right-side)');

figure(4)
plot(t,fly,'k'); %predicted noise
xlabel('seconds'); title('predicted noise');
