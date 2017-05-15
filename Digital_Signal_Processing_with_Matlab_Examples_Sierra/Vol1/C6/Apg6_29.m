% Read quake data file
%
fer=0;
while fer==0,  
fid2=fopen('quake.txt','r');
if fid2==-1, disp('read error') 
else quake1=fscanf(fid2,'%f \r\n'); fer=1;
end;
end;
fclose('all');

Ns=length(quake1);
t=0:0.01:((Ns-1)*0.01); %sampling times

plot(t,quake1,'k'); %plots earthquake vertical acceleration
title('earthquake vertical acceleration (cm/sec^2)'); xlabel('seconds');
