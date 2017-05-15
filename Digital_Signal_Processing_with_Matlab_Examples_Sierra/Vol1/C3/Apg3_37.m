% Quarterly US personal consumption expediture
% Display of data

% read data file
fer=0;
while fer==0,  
fid2=fopen('consum.txt','r');
if fid2==-1, disp('read error') 
else CM=fscanf(fid2,'%f \r\n'); fer=1;
end;
end;
fclose('all');

x=diff(CM); %data differencing
L=length(CM)-1;
nn=1:L;
d(nn)=x(nn)./CM(nn);
p=d*100; %percentage change

figure(1)
subplot(1,2,1)
plot(CM,'k'),
xlabel('trimester')
ylabel('Consumption')
title('US personal consumption');
axis([0 L+1 0 14000]);
subplot(1,2,2)
plot(p,'k')
xlabel('trimester')
ylabel('percentage change');
axis([0 L+1 -3 8]);

% MA(3) parameter estimation:
D=std(p);
M=mean(p);
Np=((p-M)/D)'; %column format

model=armax(Np,[0 3]);
% extract info from model structure
[A,B,C,D,F,LAM,T]=th2poly(model);
% print vector of MA(3) coeffs:
C