% Sawtooth signals 
fy=100; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s
duy=0.03; %signal duration in seconds

fs=20000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(duy-tiv); %time intervals set

y=sawtooth(wy*t,0.1); %signal data set (width 0.1)
subplot(2,2,1); plot(t,y,'k'); %plots figure
  axis([0 duy -1.5 1.5]);
  xlabel('seconds'); title('sawtooth signal');
  
y=sawtooth(wy*t,0.3); %signal data set (width 0.3)
subplot(2,2,2); plot(t,y,'k'); %plots figure
  axis([0 duy -1.5 1.5]);
  xlabel('seconds'); title('sawtooth signal');
  
y=sawtooth(wy*t,0.5); %signal data set (width 0.5)
subplot(2,2,3); plot(t,y,'k'); %plots figure
  axis([0 duy -1.5 1.5]);
  xlabel('seconds'); title('sawtooth signal');
  
y=sawtooth(wy*t,0.9); %signal data set (width 0.9)
subplot(2,2,4); plot(t,y,'k'); %plots figure
  axis([0 duy -1.5 1.5]);
  xlabel('seconds'); title('sawtooth signal');

