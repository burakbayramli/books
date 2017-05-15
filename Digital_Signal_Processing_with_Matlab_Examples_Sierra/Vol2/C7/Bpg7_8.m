%PCA of 3D accelerometer
ha=30; va=60; %horiz. & vert. direction angles (degrees)

%radians
rha=(ha*pi)/180; rva=(va*pi)/180;
%direction projections
diz=sin(rva); 
dix=cos(rva)*sin(rha);
diy=cos(rva)*cos(rha);

%acceleration signal, in the direction ha-hv
t=0:0.01:20;
N=length(t);
w=10; %frequency in rad/s
ac=cos(w*t);
%signal projections+noise
acx=(ac*dix)+(0.1*normrnd(0,1,1,N));
acy=(ac*diy)+(0.1*normrnd(0,1,1,N));
acz=(ac*diz)+(0.1*normrnd(0,1,1,N));

%PCA computation
M=3;
D=zeros(M,N);
D(1,:)=acx;
D(2,:)=acy;
D(3,:)=acz;

%PCA---------------
me=mean(D,2); %mean of each row
X=D-repmat(me,1,N); %subtract mean in each row
A=X'/sqrt(N-1);
%singular value decomposition
[U,S,V]=svd(A); %V contains principal components 

%display-----------------------
%the accelerometer signals
figure(1)
subplot(3,1,1); plot(t,acx,'k'); ylabel('acx'); 
title('signals from 3D accelerometer');
subplot(3,1,2); plot(t,acy,'k'); ylabel('acy');
subplot(3,1,3); plot(t,acz,'k'); ylabel('acz');
xlabel('seconds');

%the main PCA component
figure(2)
subplot(3,1,1); plot(acx,acy,'g.'); hold on;
plot([-V(1,1) V(1,1)],[-V(2,1) V(2,1)],'r');
xlabel('acx'); ylabel('acy'); title('main PCA component');
subplot(3,1,2); plot(acx,acz,'g.'); hold on;
plot([-V(1,1) V(1,1)],[-V(3,1) V(3,1)],'r');
xlabel('acx'); ylabel('acz');
subplot(3,1,3); plot(acy,acz,'g.'); hold on;
plot([-V(2,1) V(2,1)],[-V(3,1) V(3,1)],'r');
xlabel('acy'); ylabel('acz');

%print values for comparison
dix/V(1,1)
diy/V(2,1)
diz/V(3,1)
