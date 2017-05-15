% Comparison of hw(n) of Hanning, Hamming and Blackman windows
N=50; %even order

hw=hanning(N+1); %Hanning window
plot(hw,'k'); hold on;
hw=hamming(N+1); %Hamming window
plot(hw,'r');
hw=blackman(N+1); %Blackman window
plot(hw,'b'); hold on;
axis([1 51 0 1.1]);
title('hw(n) of 50th Hanning, Hamming and Blackman');
xlabel('n');
