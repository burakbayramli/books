% Gold prices
% Display of data

% read data file
fer=0;
while fer==0,  
fid2=fopen('gold.txt','r');
if fid2==-1, disp('read error') 
else GL=fscanf(fid2,'%f \r\n'); fer=1;
end;
end;
fclose('all');

lgGL=log(GL); %logarithm of the data

x=diff(lgGL); %differencing of log(data)
L=length(x);

figure(1)
subplot(1,3,1)
plot(GL,'k'),
xlabel('month')
ylabel('price')
title('Gold price');
axis([0 L+1 200 1900]);

subplot(1,3,2)
plot(lgGL,'k')
xlabel('month')
ylabel('logarithm of price');
axis([0 L+1 5.5 7.5]);

subplot(1,3,3)
plot(x,'k')
xlabel('month')
ylabel('returns');
axis([0 L -0.15 0.15]);

% MA(3) parameter estimation:
D=std(x);
M=mean(x);
Nx=((x-M)/D);

model=armax(Nx,[7 10]);
% extract info from model structure
[A,B,C,D,F,LAM,T]=th2poly(model);
% print vector of ARMA(7 10) coeffs:
A
C