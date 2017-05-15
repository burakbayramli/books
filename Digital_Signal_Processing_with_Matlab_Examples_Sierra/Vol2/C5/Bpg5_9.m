% LMS
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

%FIR coeff. computation
Nh=50; %number of FIR coefficients
wh=zeros(Nh,1); %FIR coefficients initialization
yy=zeros(Nh,1); %y vector
er=zeros(1,Nx); %for error recording
hwh=zeros(1,Nx); %for coefficient recording
fo=zeros(1,Nx); %filter output

hwh(1)=wh(1); %initial value

mu=1/(100*Nh); %learning rate
for nn=Nh:Nx-1,
   %actualization of yy
   yy=y(nn:-1:(nn-Nh+1))'; %take a segment and reverse
   %filter output
   fo(nn)=wh'*yy;
   %error
   er(nn)=x(nn)-fo(nn);
   %new FIR coeffs   
   wh=wh+((mu*er(nn))*yy);   
   hwh(nn+1)=wh(1); %record of new wh(1)
end;  

%append for FIR symmetry
rwh=wh';
lwh=fliplr(rwh); 
sh=0.5*[lwh rwh]; %symmetrical FIR

fly=filter(sh,1,y); %apply the Wiener filter

%display---------------------------

figure(1)
plot(er,'k'); %error evolution
title('error evolution');

figure(2)
plot(wh,'k'); %FIR coefficients
title('FIR coefficients (right-side)');

figure(3)
np=60*20;
plot(t(1:np),fly(1:np),'k'); %filtered signal
xlabel('seconds'); title('filtered signal');

figure(4)
plot(hwh,'k'); %evolution of wh(1)
title('evolution of filter coefficient h(1)');
