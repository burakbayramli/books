% Comparing original and computed acceleration direction
pa=70; %perspective angle (degrees)
ha=30; va=60; %horiz. & vert. direction angles (degrees)

%radians
rpa=(pa*pi)/180;
rha=(ha*pi)/180; rva=(va*pi)/180;

%direction projections
diz=sin(rva); 
dix=cos(rva)*sin(rha);
diy=cos(rva)*cos(rha);
%in perspective
ddx=(dix*cos(rpa));
ddy=(dix*sin(rpa));
px=diy-ddx;
py=diz-ddy;

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

%display-------------------------------------------------
%axes
plot([0 0],[0 1],'b'); hold on; %z axis
plot([-cos(rpa) 0],[-sin(rpa) 0],'b'); %x axis
plot([0 1],[0 0],'b'); %y axis

%direction of original signal
plot([0 px],[0 py],'k');
%projection
plot([0 px],[0 -ddy],'k-.');
plot([px px],[py -ddy],'k-.');
plot([-ddx px],[-ddy -ddy],'k:');
plot([px px+ddx],[-ddy 0],'k:');

%direction of main PCA component
%direction projections
aix=V(1,1); aiy=V(2,1); aiz=V(3,1);
%in perspective
adx=(aix*cos(rpa));
ady=(aix*sin(rpa));
ax=aiy-adx;
ay=aiz-ady;
plot([0 ax],[0 ay],'r');
title('direction of measured acceleration');
