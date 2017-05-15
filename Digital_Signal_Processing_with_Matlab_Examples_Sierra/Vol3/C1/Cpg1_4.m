%Example of satellite position at T0
%
%reference position at T0
r0=600;
alpha0=pi/2;

Np=200; %number of points to display

%reserve space for points
px=zeros(1,Np);
py=zeros(1,Np);

%influence of perturbations and noise
%variances
sigr=10;
siga=0.45; %radians

nr=sigr*randn(1,Np);
na=siga*randn(1,Np);

for nn=1:Np,
   r=r0+nr(nn);
   a=alpha0+na(nn);
   px(nn)=r*cos(a);
   py(nn)=r*sin(a);
end;  

xmean=sum(px/Np);
ymean=sum(py/Np);

%display
figure(1)
plot(px,py,'r.'); hold on; %the points
plot([0 0],[0 r0+20],'b'); %vertical line
plot(0,r0,'k+','MarkerSize',12); %+ for reference satellite
%
%X for mean cartesian position:
plot(xmean,ymean,'kx','MarkerSize',12);
%
title('Satellite position data');


