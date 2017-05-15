% Comparison of hw(n) of Kaiser window
fs=130; %sampling frequency in Hz.
N=50; %even order

beta=1; %filter parameter
hw=kaiser(N+1,beta); %Kaiser window
plot(hw,'k'); %plots hw(n)
hold on; 

for beta=2:8,
hw=kaiser(N+1,beta); %Kaiser window
plot(hw,'k'); %plots hw(n)
end

axis([1 51 0 1.2]); 
title('50th Kaiser hw(n)'); xlabel('n');
