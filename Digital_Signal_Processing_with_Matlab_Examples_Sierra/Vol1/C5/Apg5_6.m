% h(n)= 13 ones, H(w) of the digital filter
n=-6:1:6; %sample times
h=ones(13,1); % vector of 13 ones
subplot(1,2,1); stem(n,h,'k'); %plots h(n)
axis([-7 7 0 1.2]); title('h(n)'); xlabel('n');

H1=real(fft(h,512)); Hf=H1/max(H1); %discrete Fourier transform
w=-pi:(2*pi/511):pi;
subplot(1,2,2); plot(w,fftshift(Hf),'k'); %plots H(w)
axis([-pi pi -0.3 1]); title('H(w)'); xlabel('normalized frequency');
hold on; 
plot([-pi pi],[0 0],'--k'); %horizontal dotted line
