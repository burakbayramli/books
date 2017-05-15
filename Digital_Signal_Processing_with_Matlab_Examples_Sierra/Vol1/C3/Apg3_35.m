%Example of weekly toothpaste sales
% Display of data

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

figure(1)
subplot(1,2,1)
plot(TP,'k'),
xlabel('weeks')
ylabel('Toothpaste')
title('weekly toothpaste sales');
subplot(1,2,2)
plot(x,'k')
xlabel('weeks')
ylabel('differences');