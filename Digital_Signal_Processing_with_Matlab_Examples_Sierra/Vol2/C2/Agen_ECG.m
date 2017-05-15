% Read ECG data file and save two beats
fs=200; %samplig frequency in Hz
tiv=1/fs; %time interval between samples

fer=0;
while fer==0,  
fid2=fopen('ECGnormal.txt','r');
if fid2==-1, disp('read error') 
else Wdat=fscanf(fid2,'%f \r\n'); fer=1;
end;
end;
fclose('all');
Ns=length(Wdat); %number of signal samples
t=0:tiv:((Ns-1)*tiv); %time intervals set

p2Wdat=Wdat(1:512); %extract two heartbeats

fer=0;
  while fer==0,  
	fid1=fopen('/aJMG/aJMG/academic/libroProcSign/PSMatlab/C10wlt/ECG_short.txt','w');
	if fid1==-1, disp('error fichero') 
	else fprintf(fid1,'%f \r\n',p2Wdat); fer=1; disp('ya');
   end;
end;
fclose('all');



%display
plot(t,Wdat,'k');
axis([0 25 6 10]);
title('normal ECG');
