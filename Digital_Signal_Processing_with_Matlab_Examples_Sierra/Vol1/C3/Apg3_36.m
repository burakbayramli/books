% AR(1) model of weekly toothpaste sales

% read data file
fer=0;
while fer==0,  
fid2=fopen('Tpaste.txt','r');
if fid2==-1, disp('read error') 
else TP=fscanf(fid2,'%f \r\n'); fer=1;
end;
end;
fclose('all');

x=diff(TP); %data differencing
D=std(x);
M=mean(x);
Nx=(x-M)/D; %normalized data

A=aryule(Nx,1); %AR(1) parameter estimation 

%AR model simulation
%coeffs. of polynomial A
a1=A(2);
%variable initial values
y=0; y1=0;

Ni=100; %number of iterations
ry=zeros(1,Ni); %for storage of y values
ee=randn(1,Ni); %vector of random values
 
%iterations
for nn=1:Ni,
    e=ee(nn);
    y=e-(a1*y1); %according with AR model
    ry(nn)=y;
    y1=y; %memory update
end;

figure(1)
plot(ry,'k');
title('evolution of model output');
xlabel('n');

