%Spectrum of central quake signal
fer=0;
while fer==0,  
fid2=fopen('quake.txt','r');
if fid2==-1, disp('read error') 
else y1=fscanf(fid2,'%f \r\n'); fer=1;
end;
end;
fclose('all');
fs1=100; %in Hz

y1central=y1(300:1100); %central quake signal
ff1=fft(y1central,fs1); %Fourier transform
plot(abs(ff1(1:50)),'k');
title('central quake signal: spectral density');
xlabel('Hz')
axis([0 25 0 13000]); grid;
