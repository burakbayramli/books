% Linear Prediction using NLMS
% sine signal + noise
fx=1; %signal frequency in Hz
wx=2*pi*fx; %signal frequency in rad/s

fs=60; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(30-tiv); %time intervals set

x=sin(wx*t); %signal data set
Nx=length(x);
v=0.1*randn(1,Nx); %normal noise
u=x+v; %sine+noise

%Delayed signal-------------------------
Nd=7; %delay length
ud=zeros(1,Nx); ud((1+Nd):Nx)=u(1:(Nx-Nd));
 
 %-------------------------------------------
Nh=Nd; %number of FIR coefficients
%FIR coeff. identification
wh=zeros(Nh,1); %FIR coefficients initialization
er=zeros(1,Nx); %for error recording
fo=zeros(1,Nx); %filter output

mu=1/(10*Nh); %learning rate

for nn=Nh:Nx-1,
   %actualization of ud
   us=ud(nn:-1:(nn-Nh+1))'; %take a segment and reverse
   %filter output
   fo(nn)=wh'*us;
   %error
   er(nn)=u(nn)-fo(nn);
   %new FIR coeffs
   ipsi=0.01; %small constant, to be edited
   aux=mu/(ipsi+norm(us));
   wh=wh+((aux*er(nn))*us);
end;  

%display---------------------------

figure(1)
plot(er,'k'); %error evolution
title('error evolution');

figure(2)
plot(wh,'k'); %the FIR coefficients
title('Identified FIR coefficients');

figure(3)
subplot(2,1,1)
plot(u,'k'); title('u signal');
subplot(2,1,2)
plot(fo,'k'); title('filter output');
