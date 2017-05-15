% Inverse System Identifications using NLMS
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

%The Unknown System-------------------------
wu=[0.1 0.15 0.2 0.3 0.2 0.15 0.1]'; 
Nh=7; %number of FIR coefficients (equal to system order)

%The delayed signal-------------------------
Nd=2*Nh; %two times the filter lenght
d=zeros(1,Nx); d((1+Nd):Nx)=u(1:(Nx-Nd));
 
%-------------------------------------------
%FIR coeff. identification
wh=zeros(Nh,1); %FIR coefficients initialization
uu=zeros(Nh,1); %u vector
er=zeros(1,Nx); %for error recording
fo=zeros(1,Nx); %filter output
so=zeros(1,Nx); %system output

mu=1/(10*Nh); %learning rate

for nn=Nh:Nx-1,
   %actualization of uu
   uu=u(nn:-1:(nn-Nh+1))'; %take a segment and reverse
   %system output
   so(nn)=wu'*uu;
   %filter output
   ss=so(nn:-1:(nn-Nh+1))';
   fo(nn)=wh'*ss;
   %error
   er(nn)=d(nn)-fo(nn);
   %new FIR coeffs
   ipsi=0.01; %small constant, to be edited
   aux=mu/(ipsi+norm(ss));
   wh=wh+((aux*er(nn))*ss);
end;  

%display---------------------------
figure(1)
plot(er,'k'); %error evolution
title('error evolution');

figure(2)
plot(wu,'b'); hold on; %the system
plot(wh,'k'); %the identified inverse
title('Identified FIR coefficients');

figure(3)
subplot(2,1,1)
plot(u,'k'); title('u signal');
subplot(2,1,2)
plot(fo,'k'); title('filter output');
