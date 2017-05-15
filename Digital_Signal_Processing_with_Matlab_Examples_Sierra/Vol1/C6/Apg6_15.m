%two added sines

fs=200; %sampling rate in Hz
t=0:(1/fs):1; %time intervals set (1 seconds)

f1=10; %sine1 frequency in Hz
f2=11; %sine2 frequency in Hz
y=sin(2*pi*f1*t)+sin(2*pi*f2*t); %sum of two sine signals
Ny=length(y);

plot(t,y,'k')
title('two added sine signals'); xlabel('seconds');
