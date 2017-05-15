% Example of AR model estimation
% Canadian Lynx data

% load Lynx population data
fer=0;
while fer==0,  
fid2=fopen('lynx.txt','r');
if fid2==-1, disp('read error') 
else lydat=fscanf(fid2,'%f \r\n'); fer=1;
end;
end;
fclose('all');
N=length(lydat);

MD=mean(lydat);
zydat=lydat-MD; %zero-mean data

% model parameter estimation
[model1,e]=aryule(zydat,20);

%response of the model to white noise
edat=filter(1,model1,sqrt(e)*randn(N,1)); 

figure(1)
plot(lydat,'k');
title('Canadian Lynx data');

figure(2)
plot(zydat,'g'); hold on
plot(edat,'k');
title('AR model response')

