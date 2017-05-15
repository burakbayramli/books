% System Identifications using NLMS

% sine signal + noise
fx=1; %signal frequency in Hz
wx=2*pi*fx; %signal frequency in rad/s

fs=60; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(15-tiv); %time intervals set

x=sin(wx*t); %signal data set
Nx=length(x);
v=randn(1,Nx); %normal noise
u=x+v; %sine+noise

%The Unknown System-------------------------
wu=[0.1 0.15 0.2 0.3 0.2 0.15 0.1]'; 

%-------------------------------------------
%FIR coeff. identification
Nh=7; %number of FIR coefficients
wh=zeros(Nh,1); %FIR coefficients initialization
uu=zeros(Nh,1); %u vector
er=zeros(1,Nx); %for error recording
fo=zeros(1,Nx); %filter output
d=zeros(1,Nx); %system output

mu=1/(10*Nh); %learning rate

for nn=Nh:Nx-1,
   %actualization of uu
   uu=u(nn:-1:(nn-Nh+1))'; %take a segment and reverse
   %filter output
   fo(nn)=wh'*uu;
   %system output
   d(nn)=wu'*uu;
   %error
   er(nn)=d(nn)-fo(nn);
   %new FIR coeffs
   ipsi=0.01; %small constant, to be edited
   aux=mu/(ipsi+norm(uu));
   wh=wh+((aux*er(nn))*uu);
end;  

%display---------------------------

figure(1)
plot(wu,'g'); hold on; %Identified FIR coefficients
plot(wh,'k');
title('Identified FIR coefficients');

figure(2)
plot(er,'k'); %error evolution
title('error evolution');


