% Australian Beer production
% Display of data

% read data file
fer=0;
while fer==0,  
fid2=fopen('beer.txt','r');
if fid2==-1, disp('read error') 
else BR=fscanf(fid2,'%f \r\n'); fer=1;
end;
end;
fclose('all');

N=length(BR);
t=(1:N)';

% estimate growth line
[r,s]=polyfit(t,BR,1);
gl=r(2)+r(1).*t; 

%subtract the line
sB=BR-gl;

%fit a sinusoid
y=sB;
f=0.2288;
ferror=inline('sum(abs(y-x*cos(f+(t*2*pi/12))))'); %function to be minimised by x
[ox ferrorx]=fminbnd(ferror,5,40,[],f,t,y); %find x for minimum error
ye=ox*cos(f+(t*2*pi/12)); %estimated cos( )

%subtract the sinusoid
nB=sB-ye;

% display -----------

figure(1)
plot(BR,'k');
xlabel('months');
ylabel('production');
title('Australia beer production');

figure(2)
subplot(4,1,1)
plot(BR,'k'),
xlabel('month')
ylabel('Beer prduction')
title('Beer production');
axis([0 N 0 250]);

subplot(4,1,2)
plot(gl,'k')
xlabel('month')
ylabel('growth line');
axis([0 N 0 250]);

subplot(4,1,3)
plot(ye,'k')
xlabel('month')
ylabel('cosine component');
axis([0 N -50 50]);

subplot(4,1,4)
plot(nB,'k')
xlabel('month')
ylabel('random component');
axis([0 N -50 50]);

figure(3)
plot(sB,'k'); hold on; plot(ye,'r');
xlabel('month');
ylabel('cos( ) fitting');
title('cos( ) fitting');

% ARMA parameter estimation:
D=std(nB);
M=mean(nB);
Ny=((nB-M)/D);

model=armax(Ny,[1 12]);
% extract info from model structure
[A,B,C,D,F,LAM,T]=th2poly(model);
% print vector of ARMA(1 12) coeffs:
A
C