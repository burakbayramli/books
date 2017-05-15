%Central limit of wav sounds

%read a set of sound files
[y1,fs]=wavread('srn01.wav'); %read wav file
[y2,fs]=wavread('srn02.wav'); %read wav file
[y3,fs]=wavread('srn04.wav'); %read wav file
[y4,fs]=wavread('srn06.wav'); %read wav file
[y5,fs]=wavread('log35.wav'); %read wav file
[y6,fs]=wavread('ORIENT.wav'); %read wav file
[y7,fs]=wavread('elephant1.wav'); %read wav file
[y8,fs]=wavread('harp1.wav'); %read wav file

%Note: all signals have in this example fs=16000

N=25000; %clip signals to this length
y=zeros(8,N); %signal set
y(1,:)=y1(1:N)'; y(2,:)=y2(1:N)'; y(3,:)=y3(1:N)'; y(4,:)=y4(1:N)';
y(5,:)=y5(1:N)'; y(6,:)=y6(1:N)'; y(7,:)=y7(1:N)'; y(8,:)=y8(1:N)';

%normalization
for nn=1:8,
   s=y(nn,:); s=s-mean(s); %zero mean
   vr=var(s); s=s/sqrt(vr); %variance=1
   y(nn,:)=s;
end; 

%sum of signals
S=sum(y);

%histogram
figure(1)
hist(S,30); colormap('cool');
title('histogram of the sum of signals');

%sound of the sum
soundsc(S,fs);
   
 