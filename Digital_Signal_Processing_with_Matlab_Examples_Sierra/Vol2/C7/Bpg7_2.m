%Exploring the fourth moment as projection axis is rotated
%example of two audio signals

%read two audio signals, they have same size
[x,fx]=wavread('wind1.wav'); %read wav file
[y,fy]=wavread('b1.wav'); %read wav file

N=length(x);
A=60; %number of circle partitions
mnt=zeros(1,A+1);
ag=zeros(1,A+1);

i=1:N; %vectorized iterations

for nn=0:A,
alpha=(2*pi*nn)/A; %angle of projection axis in radians
p=zeros(1,N);
%projection of scatterplot on the inclined axis
   p(i)=(x(i)*cos(alpha)) + (y(i)*sin(alpha));
   
moment4=mean(p.^4); %fourth moment
mnt(nn+1)=moment4; %save result
ag(nn+1)=alpha;
end;

%display
figure(1)
polar(ag,mnt,'k');
title('fourth moment as projection axis rotates');
