%Periodogram of Sunspots
% Read data file
%
fer=0;
while fer==0,  
fid2=fopen('sunspots.txt','r');
if fid2==-1, disp('read error') 
else Ssp=fscanf(fid2,'%f \r\n'); fer=1;
end;
end;
fclose('all');

%detrended (differenced) data
x=diff(Ssp);
N=length(x);
M=N/2;

figure(1)
plot(Ssp,'k'); %plots Sunspot data
title('Sunspots'); xlabel('index');

figure(2)
P=periodogram(x); 
Pn=P/(2*sqrt(N));
freq=(0:M)/N;
plot(freq,Pn(1:M+1),'k')
title('Periodogram');
xlabel('freq');