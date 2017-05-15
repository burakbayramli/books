% Kurtosis projection pursuit
% example of two mixed speeches

%read two sound files
[a,fs1]=wavread('spch1.wav'); %read wav file
[b,fs1]=wavread('spch2.wav'); % "  "  "
s1=(a-mean(a))'; %zero-mean
s2=(b-mean(b))'; % " "  "
vr1=var(s1); s1=s1/sqrt(vr1); %variance=1
vr2=var(s2); s2=s2/sqrt(vr2); %"  "  "

s=[s1;s2]; %combine sources

%mix of sources
N=length(s1); 
M=[0.7 0.3; 0.3 0.7]; %example of mixing matrix
x=M*s; %mixed signals

N=length(a);
A=60; %number of circle partitions
kur=zeros(1,A+1);
ag=zeros(1,A+1);

i=1:N; %vectorized iterations

for nn=0:A,
alpha=(2*pi*nn)/A; %angle of projection axis in radians
p=zeros(1,N);
%projection of scatterplot on the inclined axis
   p(i)=(x(1,i)*cos(alpha)) + (x(2,i)*sin(alpha));
   
moment4=mean(p.^4); %fourth moment
moment2=mean(p.^2); %second moment
kst=moment4-(3*(moment2.^2)); %kurtosis
kur(nn+1)=kst; %save result
ag(nn+1)=alpha;
end;

%display
%scatterplot
figure(1)
plot(x(1,:),x(2,:),'k.'); hold on; %scatterplot
L=2.7;
plot([-L L],[0 0],'k');
plot([0 0],[-L L],'k');
axis([-L  L -L L]);
title('scatterplot: 2 mixed speeches'); 
xlabel('x'); ylabel('y');

%pursuit
figure(2)
polar(ag,kur,'k');
title('kurtosis as projection axis rotates');

