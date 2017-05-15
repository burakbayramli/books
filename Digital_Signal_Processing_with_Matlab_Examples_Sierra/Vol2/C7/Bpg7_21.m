% Kurtosis projection pursuit
% example of two original speeches

%read two sound files
[a,fs1]=wavread('spch1.wav'); %read wav file
[b,fs1]=wavread('spch2.wav'); % "  "  "
s1=(a-mean(a))'; %zero-mean
s2=(b-mean(b))'; % " "  "
vr1=var(s1); s1=s1/sqrt(vr1); %variance=1
vr2=var(s2); s2=s2/sqrt(vr2); %"  "  "

s=[s1;s2]; %combine sources

N=length(a);
A=60; %number of circle partitions
kur=zeros(1,A+1);
ag=zeros(1,A+1);

i=1:N; %vectorized iterations

for nn=0:A,
alpha=(2*pi*nn)/A; %angle of projection axis in radians
p=zeros(1,N);
%projection of scatterplot on the inclined axis
   p(i)=(s(1,i)*cos(alpha)) + (s(2,i)*sin(alpha));
   
moment4=mean(p.^4); %fourth moment
moment2=mean(p.^2); %second moment
kst=moment4-(3*(moment2.^2)); %kurtosis
kur(nn+1)=kst; %save result
ag(nn+1)=alpha;
end;

%display
%pursuit
figure(2)
polar(ag,kur,'k');
title('kurtosis as projection axis rotates');

