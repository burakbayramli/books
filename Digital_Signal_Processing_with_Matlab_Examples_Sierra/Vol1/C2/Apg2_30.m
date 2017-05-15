%Histogram of Bimodal distribution
% Geyser eruption data (time between esruptions)

%read data
fer=0;
while fer==0,  
fid2=fopen('Geyser1.txt','r');
if fid2==-1, disp('read error') 
else y1=fscanf(fid2,'%f \r\n'); fer=1;
end;
end;
fclose('all');

%display
hist(y1,30); colormap('cool');
title('Time between Geyser eruptions');