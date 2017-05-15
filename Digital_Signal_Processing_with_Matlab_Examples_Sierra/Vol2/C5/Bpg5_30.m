% ECG example

fs=1000; %samplig frequency in Hz
tiv=1/fs; %time interval between samples

%read ECG files
fer=0;
while fer==0,  
fid2=fopen('ecg_mother.txt','r');
if fid2==-1, disp('read error') 
else Mecg=fscanf(fid2,'%f \r\n'); fer=1;
end;
end;
fclose('all');

fer=0;
while fer==0,  
fid2=fopen('ecg_both.txt','r');
if fid2==-1, disp('read error') 
else Becg=fscanf(fid2,'%f \r\n'); fer=1;
end;
end;
fclose('all');

Nx=length(Mecg); %number of signal samples
t=0:tiv:((Nx-1)*tiv); %time intervals set

%prepare for filtering
Nh=121; %number of FIR filter taps
y=Becg;
x=Mecg;

G=convmtx(x,Nh); %model of Mecg(T) --> Mecg(A)
G=G((Nh-1)/2 + [1:Nx],:); %for zero delay
xe=G\y; %estimated Mecg using Becg
f=y-(G*xe); %fetal ECG

%display----------
figure(1)
subplot(2,1,1)
plot(t,x,'k'); title('Thorax ECG');
subplot(2,1,2)
plot(t,y,'k'); title('Abdominal ECG');
xlabel('s');

figure(2)
plot(t,f,'k'); title ('fetal ECG');
xlabel('s');