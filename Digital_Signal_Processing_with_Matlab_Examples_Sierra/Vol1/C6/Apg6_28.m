% ADSR synthesis of audio sine signal
fs=30000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(1-tiv); %time intervals set (1 second)
fC=440; %C note in Hz
fE=659; %E note in Hz
fG=784; %G note in Hz

%setting the ADSR envelope:
NA=fs/5; evA=zeros(1,NA); %evA during 0.2 seconds
for nn=1:NA,
   evA(nn)=2*(nn/NA); %evA linear increase 
end
ND=fs/5; evD=zeros(1,ND); %evD during 0.2 seconds
for nn=1:ND,
   evD(nn)=2-(nn/ND); %evD linear decrease 
end
NS=fs/5; evS=zeros(1,NS); %evS during 0.2 seconds
for nn=1:NS,
   evS(nn)=1; %evS constant 
end
NR=2*fs/5; evR=zeros(1,NR); %evR during 0.4 seconds
for nn=1:NR,
   evR(nn)=1-(nn/NR); %evR linear decrease 
end

evT=[evA,evD,evS,evR]; %the total envelope

%C ADSR note
yC=evT.*sin(2*pi*fC*t);
%E ADSR note
yE=evT.*sin(2*pi*fE*t);
%G ADSR note
yG=evT.*sin(2*pi*fG*t);

%playing the notes
soundsc(yC,fs);
pause(0.5);
soundsc(yG,fs);
pause(0.5);
soundsc(yE,fs);

plot(t,yC,'g'); %plots the C ADSR signal
axis([0 1 -2.2 2.2]);
xlabel('seconds'); title('ADSR sine signal');
