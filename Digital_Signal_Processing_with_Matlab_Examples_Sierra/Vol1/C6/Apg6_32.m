% Read ECG data file and compute autocovariance
fs=200; %samplig frequency in Hz
tiv=1/fs; %time interval between samples

fer=0;
while fer==0,  
fid2=fopen('ECGnormal.txt','r');
if fid2==-1, disp('read error') 
else Wdat=fscanf(fid2,'%f \r\n'); fer=1;
end;
end;
fclose('all');
Ns=length(Wdat); %number of signal samples
t=0:tiv:((Ns-1)*tiv); %time intervals set

subplot(2,1,1) %signal plot
plot(t,Wdat,'k');
axis([0 25 6 10]);
title('normal ECG');

subplot(2,1,2) %autocovariance plot
av=xcov(Wdat); %signal autocovariance
fiv=1/fs; %frequency interval between harmonics
hf=0:fiv:((fs/2)-fiv); %set of harmonic frequencies
plot(t(1:Ns),av(Ns:((2*Ns)-1)),'k');  %plots autocovariance
xlabel('seconds'); title('autocovariance');
axis([0 25 -300 700]);

%find heart beat frequency:
[M K]=max(av((Ns+10):(2*Ns)-1)); %maximum (not DC) in autocovariance
TW=hf(K); %period corresponding to maximum
FW=(1/TW)*60; %frequency of heart beat in puls/min
nfw=num2str(FW); %convert to string format
msg=['heart beat per min. = ',nfw];
disp(msg); %message