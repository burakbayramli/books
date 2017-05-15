% Read ECG data file and compute autocovariance
fs=200; %samplig frequency in Hz
tiv=1/fs; %time interval between samples

fer=0;
while fer==0,  
fid2=fopen('ECGproblem.txt','r');
if fid2==-1, disp('read error') 
else Wdat=fscanf(fid2,'%f \r\n'); fer=1;
end;
end;
fclose('all');
Ns=length(Wdat); %number of signal samples
t=0:tiv:((Ns-1)*tiv); %time intervals set

subplot(2,1,1) %signal plot
plot(t,Wdat,'k');
axis([0 25 3 12]);
title('ECG with problem');

subplot(2,1,2) %autocovariance plot
av=xcov(Wdat); %signal autocovariance
plot(t(1:Ns),av(Ns:((2*Ns)-1)),'k');  %plots autocovariance
xlabel('seconds'); title('autocovariance');
axis([0 25 -1000 1200]);

